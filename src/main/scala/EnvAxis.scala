import model.{Axis, AxisMember}


abstract class EnvAxis()(implicit val axis: Axis[AxisMember]) extends AxisMember


object EnvAxis extends Axis[EnvAxis] {



  override def name: String = "env"

  case object Production extends EnvAxis {
  }

  case object Dummy extends EnvAxis {
  }
}

abstract class RepoAxis()(implicit val axis: Axis[AxisMember]) extends AxisMember


object RepoAxis extends Axis[RepoAxis] {


  override def name: String = "repo"

  case object Production extends RepoAxis {
    override def activatedBy: Set[AxisMember] = Set(EnvAxis.Production)

  }

  case object Dummy extends RepoAxis {
    override def activatedBy: Set[AxisMember] = Set(EnvAxis.Dummy)
  }
}


abstract class ImplAxis()(implicit val axis: Axis[AxisMember]) extends AxisMember

object ImplAxis  extends Axis[ImplAxis] {


  override def name: String = "impl"

  case object Cassandra extends ImplAxis {
    override def activatedBy: Set[AxisMember] = Set(EnvAxis.Production)
  }

  case object Dynamo extends ImplAxis {
    override def activatedBy: Set[AxisMember] = Set(EnvAxis.Production)
  }

  case object Memory extends ImplAxis {
    override def activatedBy: Set[AxisMember] = Set(EnvAxis.Dummy)
  }
}
