package emitter

import ir.LCLanguage._
import par.TokenTypes._

object LCEmitter {
  def emitOp(op: BuiltinOperator) = op match {
    case Addition    => "+"
    case Subtraction => "-"
    case Equallity   => "="
    case Inequallity => "!="
    case Leq => "<"
    case Geq => ">"
  }

  def emitGraphMachine(e: LCExp): String = e match {
    case LCName(x)                         => x
    case LCFunction(_, n, e)               => s"(${emitGraphMachine(n)}.${emitGraphMachine(e)})"
    case LCApplication(l: LCExp, r: LCExp) => s"((${emitGraphMachine(l)})(${emitGraphMachine(r)}))"
    case LCTerminalOperation(l, op, r)     => s"(((${emitOp(op)})(${emitGraphMachine(l)}))(${emitGraphMachine(r)}))"
    case LCString(s)                       => ("\"" + s + "\"")
    case LCNumber(n)                       => n.toString
    case LCIf(e, f, s) => s"if (${emitGraphMachine(e)}) (${emitGraphMachine(f)}) (${emitGraphMachine(s)})"
    case _                                 => ""
  }

  def emitScheme(e: LCExp): String = e match {
    case LCName(x)                         => x
    case LCFunction(_, n, e)               => s"(lambda (${n.name}) ${emitScheme(e)})"
    case LCApplication(l: LCExp, r: LCExp) => s"(${emitScheme(l)} ${emitScheme(r)})"
    case LCTerminalOperation(l, op, r)     => s"(${emitOp(op)} ${emitScheme(l)} ${emitScheme(r)})"
    case LCString(s)                       => ("\"" + s + "\"")
    case LCNumber(n)                       => n.toString
    case LCIf(e, f, s) => s"(if ${emitScheme(e)} ${emitScheme(f)} ${emitScheme(s)})"
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

  def emitCpp(e: LCExp): String = e match {
    case LCName(x)                         => x
    case LCFunction(_, n, e)               => s"[=](auto ${n.name}) {\n return ${emitCpp(e)};\n }"
    case LCApplication(l: LCExp, r: LCExp) => s"(${emitCpp(l)})(${emitCpp(r)})"
    case LCTerminalOperation(l, op, r)     => s"((${emitCpp(l)}) ${emitOp(op)} (${emitCpp(r)}))"
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
