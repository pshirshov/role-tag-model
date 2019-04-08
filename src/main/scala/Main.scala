import model._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}


object Main {

  implicit class StringSeqTools[T](s: Iterable[T]) {
    def nice(): String = s.mkString("\n  - ", "\n  - ", "")
  }

  def main(args: Array[String]): Unit = {
    run(Seq(
      "env:production",
      "repo:dummy",
      "impl:memory",
      "myapp.KeyName:ImplName"
    ))

    run(Seq(
      "env:production",
    ))
  }

  private def run(a: Seq[String]) = {
    Try {
      println("-" * 120)
      val declarations = Seq(
        Declaration(Key("myapp.UserRepository"), "UserDummyRepository", Set(Tag.AxisTag(RepoAxis.Dummy), Tag.AxisTag(ImplAxis.Memory))),
        Declaration(Key("myapp.UserRepository"), "UserProductionRepositoryCassandra", Set(Tag.AxisTag(RepoAxis.Production), Tag.AxisTag(ImplAxis.Cassandra))),
        Declaration(Key("myapp.UserRepository"), "UserProductionRepositoryDynamo", Set(Tag.AxisTag(RepoAxis.Production), Tag.AxisTag(ImplAxis.Dynamo))),

        Declaration(Key("myapp.CompanyRepository"), "CompanyDummyRepository", Set(Tag.AxisTag(RepoAxis.Dummy))),
        Declaration(Key("myapp.CompanyRepository"), "CompanyProductionRepositoryCassandra", Set(Tag.AxisTag(RepoAxis.Production), Tag.AxisTag(ImplAxis.Cassandra))),
        Declaration(Key("myapp.CompanyRepository"), "CompanyProductionRepositoryDynamo", Set(Tag.AxisTag(RepoAxis.Production), Tag.AxisTag(ImplAxis.Dynamo))),
      )

      val allAxisMembers = declarations.flatMap(_.tags.flatMap {
        case Tag.AxisTag(choice) =>
          choice.activatedBy ++ choice.deactivatedBy + choice
      })
        .toSet
      val allAxis = allAxisMembers.map(_.axis)
      val grouped = allAxisMembers.groupBy(_.axis)

      val repr = grouped.map(kv => s"${kv._1}: ${kv._2.mkString(", ")}")
      println(s"All known axis: ${repr.nice()}\n")

      val parameters = parse(a, allAxisMembers)
      println(s"Config: ${parameters.nice()}\n")


      val resolved = resolve(declarations, parameters)
      println(s"Resolved conflicts: ${resolved.nice()}\n")
    } match {
      case Failure(exception) =>
        exception.printStackTrace()
      case Success(_) =>
    }
  }

  def resolve(declarations: Seq[Declaration], parameters: Seq[Parameter]): Map[Key, Declaration] = {
    val overrides = getOverrides(parameters)
    val resolved = mutable.ArrayBuffer.empty[Tuple2[Key, Declaration]]
    val unresolved = mutable.ArrayBuffer.empty[Tuple2[Key, Seq[Declaration]]]

    declarations.groupBy(_.key)
      .foreach {
        case (k, decl :: Nil) =>
          resolved.append(k -> decl)
        case (k, decls) =>
          val maybeOver = for {
            overr <- overrides.get(k)
            decl <- decls.filter(d => d.key == overr.key) match {
              case d :: Nil =>
                Some(d)
              case Nil =>
                None
              case _ =>
                ???
            }
          } yield {
            decl
          }

          maybeOver match {
            case Some(value) =>
              resolved.append(k -> value)
            case None =>
              unresolved.append(k -> decls)
          }


      }


    val r2 = resolve2(unresolved.groupBy(_._1).mapValues(_.flatMap(_._2).toSeq), parameters.collect({ case p: Parameter.AxisDef => p }))
    val all = resolved ++ r2
    val (bad, good) = all.groupBy(_._1).partition(_._2.size > 1)
    if (bad.nonEmpty) {
      throw new IllegalArgumentException(s"Conflicting bindings: $bad")
    }
    good.mapValues(_.head._2)
  }

  def findActive(active: Set[AxisMember], inactive: Set[AxisMember], unresolved: Set[AxisMember]): (Set[AxisMember], Set[AxisMember]) = {
    if (unresolved.isEmpty) {
      (active, inactive)
    } else {
      val newInactive = unresolved.filter(m => m.deactivatedBy.exists(active.contains) || active.exists(a => a.axis == m.axis && a != m)).toSet
      val newActive = unresolved.filter(m => (m.activatedBy.nonEmpty && m.activatedBy.forall(active.contains)) || active.contains(m)).toSet -- newInactive
      val allNew = newInactive ++ newActive
      if (allNew.nonEmpty) {
        findActive(active ++ newActive, inactive ++ newInactive, unresolved.diff(allNew))
      } else {
        val uniqueOnAxis = unresolved.groupBy(_.axis).filter(_._2.size == 1)
        if (uniqueOnAxis.nonEmpty) {
          val newActiveWithUnique = active ++ uniqueOnAxis.flatMap(_._2)
          findActive(newActiveWithUnique, inactive, unresolved.diff(newActiveWithUnique))
        } else {
          println(
            s"""Failure:
               |current active     = $active
               |current inactive   = $inactive
               |current unresolved = $unresolved
               |new active         = $newActive
               |new inactive       = $newInactive
             """.stripMargin)
          (active, inactive)
          //throw new IllegalStateException(s"Cannot resolve: $unresolved")
        }

      }

    }
  }

  def resolve2(unresolved: Map[Key, Seq[Declaration]], defs: Seq[Parameter.AxisDef]): Map[Key, Declaration] = {
    val allTags = unresolved.flatMap(_._2.flatMap(_.tags.map {
      case Tag.AxisTag(choice) =>
        choice
    })).toSet

    val (activeTags, inactiveTags) = findActive(defs.map(_.choice).toSet, Set.empty, allTags)
    println(s"active  : ${activeTags.mkString(",")}")
    println(s"inactive: ${inactiveTags.mkString(",")}")
    println()

    val bad1 = activeTags.intersect(inactiveTags)
    if (bad1.nonEmpty) {
      throw new IllegalArgumentException(s"Conflicting definitions: $bad1")
    }

    val (good, bad2) = activeTags.groupBy(_.axis).partition(_._2.size == 1)
    if (bad2.nonEmpty) {
      val repr = bad2.map(kv => s"${kv._1}  = ${kv._2.mkString("{", " | ", "}")}")
      println(s"Unset axis: ${repr.nice()}\n")
      throw new IllegalArgumentException(s"Unset axis: $bad2")

    }

    val out = unresolved.map {
      case (k, v) =>
        val withoutInactive = v.filter(d => d.tags.forall {
          case Tag.AxisTag(t) =>
            !inactiveTags.contains(t)
        })

        val (activated, unset) = withoutInactive.partition(d => d.tags.forall {
          case Tag.AxisTag(t) =>
            activeTags.contains(t)
        })

        if (activated.nonEmpty) {
          k -> activated
        } else {
          k -> unset
        }

    }

    val tooMany = out.filter(_._2.size > 1)
    if (tooMany.nonEmpty) {
      throw new IllegalArgumentException(s"Conflicting bindings: $tooMany")
    }
    val wiped = out.filter(_._2.isEmpty)
    if (wiped.nonEmpty) {
      throw new IllegalArgumentException(s"Wiped: $wiped")
    }

    out.mapValues(_.head)
  }

  private def getOverrides(parameters: Seq[Parameter]) = {
    val (good, bad) = parameters.collect({ case p: Parameter.OverrideDef => p }).groupBy(_.key).partition(_._2.size == 1)
    if (bad.nonEmpty) {
      throw new IllegalArgumentException(s"Conflicting overrides: $bad")
    }
    good.mapValues(_.head)
  }

  def parse(a: Seq[String], allAxis: Set[AxisMember]): Seq[Parameter] = {
    val axis = allAxis.groupBy(_.axis)

    a.map {
      arg =>
        val parts = arg.split(':')
        val id = parts.head
        val choice = parts.last
        axis.find(_._1.name == id) match {
          case Some(value) =>
            Parameter.AxisDef(value._1, value._2.find(_.id == choice).get)
          case None =>
            Parameter.OverrideDef(Key(id), choice)
        }
    }
  }
}
