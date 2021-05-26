package ir.trans

import cats.data.NonEmptyList

object IntoLC {
  import par.TokenTypes._
  import runtime.ReductionMachine.LCWithTrimmers._

  type ConstructorOrdering = Map[String, NonEmptyList[String]]

  def constructOrdering(td: TypeDeclaration): ConstructorOrdering = {
    val ordering: NonEmptyList[String] = td.expr.types.map(_.name)
    val asLookup: Map[String, NonEmptyList[String]] = ordering.map(k => k -> ordering).toList.toMap
    asLookup
  }

  def extendWithType(program: LCTExp, td: TypeDeclaration, co: ConstructorOrdering): LCTExp = {
    val prefix = "on"

    def body(on: String, params: List[String]): LCTExp = {
      // handler first
      val handled = params.foldLeft[LCTExp](LCTVar(prefix + on)){ case (accum, next) =>
        LCTApplication(accum, LCTVar(next))
      }

      //then parameters
      co.get(on).get.reverse.foldLeft(handled){ case (accum, next) =>
        LCTAbstration(prefix + next, accum)
      }
    }

    //add constructors
    td.expr.types.foldLeft(program){ case (accum, next) =>
      val params: List[String] = next.ids.zipWithIndex.map{ case (_, i) => s"${i}_x" }
      val b = body(next.name, params)
      val abs = params.reverse.foldLeft(b){ case (accum, next) =>
        LCTAbstration(next, accum)
      }
      LCTLet(next.name, abs, accum)
    }
  }

  def funToLet(program: LCTExp, fd: FunDecl, functionsInScope: NonEmptyList[String], co: ConstructorOrdering)(toBindUnder: String): LCTExp = {
    //std function handling
    val b = funBodyToLC(fd.body, co)
    val withParams = fd.params.reverse.foldLeft(b){ case (accum, next) =>
      LCTAbstration(next.id, accum)
    } 
    
    val newFunNames = functionsInScope.map(name => s"$name''")

    //then add bindings let g = g'' f'' g'' in let f = f'' f'' g'' in ...
    val withBindings = functionsInScope.foldLeft(withParams){ case (accum, next) =>
      //add the lets
      val appsForThis = newFunNames.foldLeft[LCTExp](LCTVar(next + "''")){ case (accum, next) =>
        LCTApplication(accum, LCTVar(next))
      }
      LCTLet(next, appsForThis, accum)
    }

    //then add params \f''->\g''->...
    val withRec: LCTExp = newFunNames.reverse.foldLeft(withBindings){ case (accum, next) => LCTAbstration(next, accum) }

    LCTLet(toBindUnder, withRec, program)
  }

  def funBodyToLC(fb: FunctionBody, co: ConstructorOrdering): LCTExp = {
    val inner = expToLC(fb.end, co)
    transform(inner, fb.children, co)
  }

  def expToLC(exp: Expression, co: ConstructorOrdering): LCTExp = exp match{
    case Apply(name, vs) => vs.foldLeft[LCTExp](LCTVar(name)){ case (accum, next) =>
      LCTApplication(accum, expToLC(next, co))
    }
    case ConstantInteger(v) => LCTNumber(v)
    case If(expr, fst, snd) => LCTIf(expToLC(expr, co), funBodyToLC(fst, co), funBodyToLC(snd,co))
    case InfixBuiltin(lhs, op, rhs) => LCTBinOp(expToLC(lhs, co), op, expToLC(rhs, co))
    case PatternMatch(expr, cases) => 
      val asL = expToLC(expr, co)
      val caseLookup = cases.map{ c => 
        val bodyE = funBodyToLC(c.body, co)
        val thisCase = c.bindings.reverse.foldLeft[LCTExp](bodyE){ case (accum, next) =>
          LCTAbstration(next, accum)
        }
        c.typeConstructor -> thisCase
      }.toList.toMap
      val ordering = co.get(cases.head.typeConstructor).get
      ordering.foldLeft(asL){ case (accum, next) =>
        LCTApplication(accum, caseLookup.get(next).get)
      }
  }

  def transform(program: LCTExp, decls: List[Declaration], co: ConstructorOrdering): LCTExp = {
    val funNames = decls.collect{ case fd: FunDecl => fd }
    //val funIntroed = 
    val lets = decls.collect{ case let: LetDecl => let }
    val inner = lets.reverse.foldLeft(program){ case (accum, next) =>
      LCTApplication(LCTAbstration(next.varname, accum), expToLC(next.value, co))
    }

    val suffix = "'"

    //add names
    val withNames = funNames.foldLeft(inner){ case (accum, next) =>
      val appiled = funNames.foldLeft[LCTExp](LCTVar(next.varname + suffix)){ case (accum, next) =>
        LCTApplication(accum, LCTVar(next.varname + suffix))
      }
      LCTLet(next.varname, appiled, accum)
    }

    //add functions
    val withBodies = funNames.foldLeft[LCTExp](withNames){ case (accum, next) =>
      funToLet(accum, next, NonEmptyList.fromListUnsafe(funNames.map(_.varname)), co)(next.varname + suffix)
    }

    withBodies
  }

  def entrypoint(decls: List[Declaration]): LCTExp = {
    val typeDecls = decls.collect{ case td: TypeDeclaration  => td }
    val cos: ConstructorOrdering = typeDecls.flatMap(constructOrdering).toMap

    val main = decls.collectFirst{ case fd@FunDecl(id, _, _) if id == "main" => fd }.get
    val innermost = expToLC(main.body.end, cos)

    val noMain = decls.filter{ 
      case FunDecl(varname, params, body) if varname == "main" => false
      case _ => true
    }

    val program = transform(innermost, noMain ++ main.body.children, cos)

    typeDecls.foldLeft(program){ case (accum, next) => 
      extendWithType(accum, next, cos)
    }
  }
}
