package ir.trans

import cats.data.NonEmptyList
import tt.LCTChecker
import scala.language.postfixOps

object IntoLC {
  import par.TokenTypes._
  import runtime.ReductionMachine.LCWithTrimmers._

  type Original = (TypelevelDeclaration, TagType)
  type TupleOrdering = (Original, NonEmptyList[String])
  type ConstructorOrdering = Map[String, TupleOrdering]

  var freshener = 1
  val charRange = 'a'.toInt to 'z'.toInt toArray
  def fresh(): String = {
    val fst = charRange(freshener % charRange.length).toChar.toString
    val suf = (freshener / charRange.length).toString
    val newName = fst + suf
    freshener = freshener + 1
    "adt_" + newName
  }

  def constructOrdering(td: TypeDeclaration): ConstructorOrdering = {
    val ordering: NonEmptyList[String] = td.expr.types.map(_.name)
    val asLookup: Map[String, TupleOrdering] =
      ordering.map(k => (k, ((td, td.expr.types.find(_.name == k).get), ordering))).toList.toMap
    asLookup
  }

  def tpToHM(tp: TypeParam) = tp match {
    case TypeName(name) =>
      if (name == "Int") {
        LCTChecker.HMConst(LCTChecker.Integer, Nil)
      } else {
        LCTChecker.HMTypeVar(name)
      }
    case ParensType(inner) =>
      def strOrFail(tp: TypeParam): Identifier = tp match {
        case TypeName(name) => name
        case x              => throw new Exception(s"nested types are not supported $x")
      }
      LCTChecker.HMConst(LCTChecker.ADT(inner.name), inner.ids.map(x => LCTChecker.HMTypeVar(strOrFail(x))))
  }
  def makeTypeRec(tt: TagType, accumulatedType: LCTChecker.HMType): LCTChecker.HMType =
    tt.ids.reverse.foldLeft(accumulatedType) {
      case (accum, next) =>
        LCTChecker.HMTypeArr(tpToHM(next), accum)
    }

  def extendWithType(program: LCTExp, td: TypeDeclaration, co: ConstructorOrdering): LCTExp = {
    val prefix = "on"

    def body(on: String, params: List[String]): LCTExp = {
      // handler first
      val handled = params.foldLeft[LCTExp](LCTVar(prefix + on)) {
        case (accum, next) =>
          LCTApplication(accum, LCTVar(next))
      }

      //then parameters
      co.get(on).get._2.reverse.foldLeft(handled) {
        case (accum, next) =>
          LCTAbstration(prefix + next, accum)
      }
    }

    //add constructors
    td.expr.types.foldLeft(program) {
      case (accum, next) =>
        val params: List[String] = next.ids.zipWithIndex.map { case (_, i) => s"${i}_x" }
        val b = body(next.name, params)
        val abs = params.reverse.foldLeft(b) {
          case (accum, next) =>
            LCTAbstration(next, accum)
        }
        new LCTLet(next.name, abs, accum) {
          override def hint = {
            val t =
              makeTypeRec(
                next,
                LCTChecker.HMConst(LCTChecker.ADT(td.typename), td.typeParams.map(x => LCTChecker.HMTypeVar(x)))
              )
            val genned = LCTChecker.gen(Map.empty, t)
            val newSubst: LCTChecker.Substitution =
              genned.bound.map(bound => LCTChecker.HMTypeVar(bound) -> LCTChecker.HMTypeVar(fresh())).toMap
            Some(Left(
                LCTChecker.substType(newSubst, genned.t)
            ))
          }
        }
    }
  }

  def funToLet(program: LCTExp, fd: FunDecl, functionsInScope: NonEmptyList[String], co: ConstructorOrdering)(
    toBindUnder: String
  ): LCTExp = {
    //std function handling
    val b = funBodyToLC(fd.body, co)
    val withParams = fd.params.reverse.foldLeft(b) {
      case (accum, next) =>
        LCTAbstration(next.id, accum)
    }

    val newFunNames = functionsInScope.map(name => s"$name''")

    //then add bindings let g = g'' f'' g'' in let f = f'' f'' g'' in ...
    val withBindings = functionsInScope.foldLeft(withParams) {
      case (accum, next) =>
        //add the lets
        val appsForThis = newFunNames.foldLeft[LCTExp](LCTVar(next + "''")) {
          case (accum, next) =>
            LCTApplication(accum, LCTVar(next))
        }
        LCTLet(next, appsForThis, accum)
    }

    //then add params \f''->\g''->...
    val withRec: LCTExp = newFunNames.reverse.foldLeft(withBindings) {
      case (accum, next) => LCTAbstration(next, accum)
    }

    LCTLet(toBindUnder, withRec, program)
  }

  def funBodyToLC(fb: FunctionBody, co: ConstructorOrdering): LCTExp = {
    val inner = expToLC(fb.end, co)
    transform(inner, fb.children, co)
  }

  def expToLC(exp: Expression, co: ConstructorOrdering): LCTExp = exp match {
    case Apply(name, vs) =>
      vs.foldLeft[LCTExp](LCTVar(name)) {
        case (accum, next) =>
          LCTApplication(accum, expToLC(next, co))
      }
    case ConstantInteger(v)         => LCTNumber(v)
    case If(expr, fst, snd)         => LCTIf(expToLC(expr, co), funBodyToLC(fst, co), funBodyToLC(snd, co))
    case InfixBuiltin(lhs, op, rhs) => LCTBinOp(expToLC(lhs, co), op, expToLC(rhs, co))
    case PatternMatch(expr, cases) =>
      val asL = expToLC(expr, co)
      val ordering = co.get(cases.head.typeConstructor).get
      //make new ADT type
      val td = ordering._1._1
      val (subst, adtT) = td match {
        case TypeDeclaration(typename, typeParams, _) => 
          //gen then inst
          val newType = LCTChecker.HMConst(LCTChecker.ADT(typename), typeParams.map(x => LCTChecker.HMTypeVar(x)))
          val genned = LCTChecker.gen(Map.empty, newType)
          val subst: LCTChecker.Substitution = genned.bound.map(bound => LCTChecker.HMTypeVar(bound) -> LCTChecker.HMTypeVar(fresh())).toMap
          (subst, LCTChecker.substType(subst, genned.t))
      }
      val caseLookup = cases
        .map { c =>
          val bodyE = funBodyToLC(c.body, co)
          val tagtype = co.get(c.typeConstructor).get._1._2
          val together = tagtype.ids.zip(c.bindings)
          val thisCase = together.reverse.foldLeft[(LCTExp, LCTChecker.Environment)]((bodyE, Map.empty)) {
            case ((accum, env), (tp, next)) =>
              (LCTAbstration(next, accum), env + (next -> LCTChecker.HMScheme(Set.empty, LCTChecker.substType(subst, tpToHM(tp)))))
          }
          (c.typeConstructor, (thisCase, bodyE))
        }
        .toList
        .toMap
      // cheat a bit and bind
      val f = "var_" + fresh()
      val folded = ordering._2.foldLeft[LCTExp](LCTVar(f)) {
        case (accum, next) =>
          LCTApplication(accum, caseLookup.get(next).get._1._1)
      }
      println(s"putting let $f = $asL in $folded")
      new LCTLet(f, asL, folded) {
        override def hint = Some(Right((adtT, caseLookup.values.toList.map{ case ((_, env), body) => (body, env) })))
      }
  }

  def transform(program: LCTExp, decls: List[Declaration], co: ConstructorOrdering): LCTExp =
    decls.reverse.foldLeft(program) {
      case (accum, next) =>
        next match {
          case FunDecl(name, params, body) =>
            LCTLet(name, params.reverse.foldLeft(funBodyToLC(body, co)) {
              case (accum, next) =>
                LCTAbstration(next.id, accum)
            }, accum)
          case LetDecl(name, value) => LCTApplication(LCTAbstration(name, accum), expToLC(value, co))
          case _                    => accum
        }
    }

  def entrypoint(decls: List[Declaration]): LCTExp = {
    val typeDecls = decls.collect { case td: TypeDeclaration => td }
    val cos: ConstructorOrdering = typeDecls.flatMap(constructOrdering).toMap

    val main = decls.collectFirst { case fd @ FunDecl(id, _, _) if id == "main" => fd }.get
    val innermost = expToLC(main.body.end, cos)

    val noMain = decls.filter {
      case FunDecl(varname, params, body) if varname == "main" => false
      case _                                                   => true
    }

    val program = transform(innermost, noMain ++ main.body.children, cos)

    typeDecls.foldLeft(program) {
      case (accum, next) =>
        extendWithType(accum, next, cos)
    }
  }
}
