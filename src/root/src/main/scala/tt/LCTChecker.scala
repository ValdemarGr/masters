package tt

object LCTChecker {
  sealed trait HMType
  final case class HMTypeVar() extends HMType

  type Environment = Map[String, HMType]
}
