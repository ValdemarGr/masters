package ir

import cats.data.NonEmptyList
import par.TokenTypes.BuiltinOperator

object LCLanguage {
  trait LCExp {
    def stringify: String
  }

  case class LCBinding(name: LCName, rhs: LCExp)
  case class LCBody(bindings: NonEmptyList[LCBinding], expr: LCExp) extends LCExp {
    def stringify: String = bindings
      .toList
      .foldLeft("") { case (accum, next) =>
        accum + s"\nlet ${next.name} = ${next.rhs.stringify}"
      } + s"\n${expr.stringify}"
  }
  case class LCName(name: String) extends LCExp {
    def stringify: String = name
  }
  case class LCFunction(metaName: String, name: LCName, exp: LCExp) extends LCExp {
    def stringify: String = s"(λ ${name.stringify}.${exp.stringify})"
  }
  case class LCApplication(fst: LCExp, snd: LCExp) extends LCExp {
    def stringify: String = s"(${fst.stringify} ${snd.stringify})"
  }
  case class LCTerminalOperation(lh: LCExp, op: BuiltinOperator, rh: LCExp) extends LCExp {
    def stringify: String = s"${lh.stringify} ${op} ${rh.stringify}"
  }
  case class LCString(v: String) extends LCExp {
    def stringify: String = s"'${v}'"
  }
  case class LCNumber(v: Int) extends LCExp {
    def stringify: String = s"${v}"
  }
  case class LCDebug(s: String) extends LCExp {
    def stringify: String = s"debug ${s}"
  }
}

