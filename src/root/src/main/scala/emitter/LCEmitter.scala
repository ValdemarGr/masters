package emitter

import ir.LCLanguage._
import par.TokenTypes._

object LCEmitter {
  def emitOp(op: BuiltinOperator) = op match {
    case Addition    => "+"
    case Subtraction => "-"
    case Equallity   => "=="
    case Inequallity => "!="
  }

  def emitScheme(e: LCExp): String = e match {
    case LCName(x)                         => x
    case LCFunction(_, n, e)               => s"(lambda (${n.name}) ${emitScheme(e)})"
    case LCApplication(l: LCExp, r: LCExp) => s"(${emitScheme(l)} ${emitScheme(r)})"
    case LCTerminalOperation(l, op, r)     => s"(${emitOp(op)} ${emitScheme(l)} ${emitScheme(r)})"
    case LCString(s)                       => ("\"" + s + "\"")
    case LCNumber(n)                       => n.toString
    case LCIf(e, f, s) => s"if ${emitScheme(e)} ${emitScheme(f)} ${emitScheme(s)}"
    case _                                 => ""
  }

  def emitHaskell(e: LCExp): String = e match {
    case LCName(x)                         => x
    case LCFunction(_, n, e)               => s"(\\${n.name} -> ${emitHaskell(e)})"
    case LCApplication(l: LCExp, r: LCExp) => s"(${emitHaskell(l)}) (${emitHaskell(r)})"
    case LCTerminalOperation(l, op, r)     => s"((${emitHaskell(l)}) ${emitOp(op)} (${emitHaskell(r)}))"
    case LCString(s)                       => ("\"" + s + "\"")
    case LCNumber(n)                       => n.toString
    case LCIf(e, f, s) => s"if (${emitHaskell(e)}) then (${emitHaskell(f)}) else (${emitHaskell(s)})"
    case _                                 => ""
    //case LCRawCode(code)                   => code
  }

  def emit(e: LCExp): String = e match {
    case LCName(x)                         => x
    case LCFunction(_, n, e)               => s"[=](auto ${n.name}) {\n return ${emit(e)};\n }"
    case LCApplication(l: LCExp, r: LCExp) => s"(${emit(l)})(${emit(r)})"
    case LCTerminalOperation(l, op, r)     => s"((${emit(l)}) ${emitOp(op)} (${emit(r)}))"
    case LCString(s)                       => ("\"" + s + "\"")
    case LCNumber(n)                       => n.toString
    case LCRawCode(code)                   => code
  }

  def emitClojure(e: LCExp): String = e match {
    case LCName(x)                         => x
    case LCFunction(_, n, e)               => s"(defn ${n.name} (${emitClojure(e)}))"
    case LCApplication(l: LCExp, r: LCExp) => s"(${emitClojure(l)})(${emitClojure(r)})"
    case LCTerminalOperation(l, op, r)     => s"( ${emitOp(op)} (${emitClojure(l)})(${emitClojure(r)}))"
    case LCString(s)                       => ("\"" + s + "\"")
    case LCNumber(n)                       => n.toString
    case LCRawCode(_)                      => ""
  }
}
