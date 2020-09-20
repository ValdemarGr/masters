package ir

import par.TokenTypes.BuiltinOperator

object LCLanguage {
  trait LCExp

  // (\a.(\b.a+b))
  case class LCName(name: String)
  case class LCFunction(name: LCName, exp: LCExp) extends LCExp
  case class LCApplication(fst: LCExp, snd: LCExp) extends LCExp
  case class LCTerminalOperation(lh: LCName, op: BuiltinOperator, rh: LCName) extends LCExp
  case class LCDebug(s: String) extends LCExp
}

