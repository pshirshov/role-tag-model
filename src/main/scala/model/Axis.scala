package model

trait Parseable[T] {
  def parse(s: String): T
}

trait AxisMember {
  def axis: Axis[AxisMember]
  def activatedBy: Set[AxisMember] = Set.empty
  def deactivatedBy: Set[AxisMember] = Set.empty
  def id: String = {
    val n = getClass.getName.toLowerCase
    n.split('$').last
  }
  override def toString: String = s"$axis/$id"

}


trait Axis[+MM <: AxisMember] {

  def name: String

  override def toString: String = s"axis:$name"

  implicit def self: Axis[AxisMember] = this

}

sealed trait Parameter

object Parameter {

  case class AxisDef(axis: Axis[AxisMember], choice: AxisMember) extends Parameter

  case class OverrideDef(key: Key, choice: String) extends Parameter

}

sealed trait Tag

object Tag {

  case class AxisTag(choice: AxisMember) extends Tag

}

case class Key(key: String)

case class Declaration(key: Key, impl: String, tags: Set[Tag])
