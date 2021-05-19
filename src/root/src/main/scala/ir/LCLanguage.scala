package ir

import par.TokenTypes._

object LCLanguage {
  trait LCExp {
    def stringify(implicit indentation: Indentation): String
    override def toString = stringify(Indentation(0))
  }

  case class Indentation(n: Int) {
    def indent: String = (0 to n).toList.map(_ => "  ").mkString
    def inc: Indentation = copy(n = n + 1)
  }

  case class LCName(name: String) extends LCExp {
    def stringify(implicit indentation: Indentation) = indentation.indent + name
  }
  case class LCLet(name: String, exp: LCExp, in: LCExp) extends LCExp {
    def stringify(implicit indentation: Indentation) = indentation.indent + s"let $name = $exp in $in"
  }
  case class LCFunction(metaName: String, name: LCName, exp: LCExp) extends LCExp {
    def stringify(implicit indentation: Indentation) = {
      val fname = indentation.indent + s"(λ${name.name}."
      val body = "\n" + s"${exp.stringify(indentation.inc)}" + "\n" + indentation.indent + s") // ${name.name} end"
      fname + body
    }
  }
  case class LCApplication(fst: LCExp, snd: LCExp) extends LCExp {
    def stringify(implicit indentation: Indentation) =
      indentation.indent + "(\n" +
        s"${fst.stringify(indentation.inc)} " + "\n" + s"${snd.stringify(indentation.inc)}" +
"\n" + indentation.indent + ")"
  }
  case class LCTerminalOperation(lh: LCExp, op: BuiltinOperator, rh: LCExp) extends LCExp {
    def stringify(implicit indentation: Indentation) =
      indentation.indent + s"(\n${lh.stringify(indentation.inc)}\n${indentation.inc.indent}${op}\n${rh.stringify(indentation.inc)})"
  }
  case class LCString(v: String) extends LCExp {
    def stringify(implicit indentation: Indentation) = indentation.indent + s"'${v}'"
  }
  case class LCNumber(v: Int) extends LCExp {
    def stringify(implicit indentation: Indentation) = indentation.indent + s"${v}"
  }
  case class LCRawCode(s: String) extends LCExp {
    def stringify(implicit indentation: Indentation) = indentation.indent + s"code ${s}"
  }
  case class LCIf(exp:LCExp, fst: LCExp, snd: LCExp) extends LCExp {
    def stringify(implicit indentation: Indentation) = indentation.indent + s""
  }
}
