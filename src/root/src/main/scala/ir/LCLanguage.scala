package ir

import par.TokenTypes.BuiltinOperator

object LCLanguage {
  trait LCExp {
    def stringify: String
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
  case class LCTerminalOperation(lh: LCName, op: BuiltinOperator, rh: LCName) extends LCExp {
    def stringify: String = s"${lh.name} ${op} ${rh.name}"
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

