package emitter

import ir.LCLanguage._
import par.TokenTypes._

object LCEmitter {
  def emitOp(op: BuiltinOperator) = op match {
    case Addition => "+"
  }

  def emit(e: LCExp): String = e match {
    case LCName(x) => x
    case LCFunction(_, n, e) => s"[=](auto ${n.name}) {\n return ${emit(e)};\n }"
    case LCApplication(l: LCExp, r: LCExp) => s"(${emit(l)})(${emit(r)})"
    case LCTerminalOperation(l, op ,r) => s"((${emit(l)}) ${emitOp(op)} (${emit(r)}))"
    case LCString(s) => ("\"" + s + "\"")
    case LCNumber(n) => n.toString
    case LCDebug(_) => ???
  }
}
