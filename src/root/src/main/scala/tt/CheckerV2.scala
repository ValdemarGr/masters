package tt

import scala.language.postfixOps
//import par.Haskelly.ADTS._

object CheckerV2 {
  //sealed trait Type
  //final case class TypeVariable(name: String, trace: String) extends Type {
    //override def toString = s"$trace [$name]"
  //}
  //final case class TypeArrow(lhs: Type, rhs: Type) extends Type {
    //override def toString = s"$lhs -> $rhs"
  //}
  //final case class TypeConstructor(name: String, params: List[Type]) extends Type {
    //override def toString = s"$name ${params.mkString(" ")}"
  //}

  //final case class Poly(boundTypes: Set[TypeVariable], t: Type) {
    //override def toString = s"forall {${boundTypes.mkString(",")}}.$t"
  //}

  //final case class ProgramVariable(name: String) {
    //override def toString = s"pv($name)"
  //}

  //final case class NamingContext(var counter: Long)

  //def ttTot(tt: TypeType)(implicit nc: NamingContext): Type = tt match {
    //case PolyType(n) =>
      //TypeVariable(n, n)
    //case ProductType(n, ts) =>
      //TypeConstructor(n, ts.map(ttTot))
    //case ArrowType(l, r) => 
      //TypeArrow(ttTot(l), ttTot(r))
  //}

  ////convert structure, then freshen and finally substitute
  //def convertPoly(tt: TypeType)(implicit nc: NamingContext): Poly = {
    //val nt = ttTot(tt)
    //val frees = freeType(nt)
    //val freshSubst = frees.map(fv => fv -> fresh(trace=fv.name)).toMap
    //val freshType = substitute(nt, freshSubst)
    //Poly(freshSubst.values.toSet, freshType)
  //}

  //val charRange = 'a'.toInt to 'z'.toInt toArray
  //def fresh(trace: String)(implicit nc: NamingContext): TypeVariable = {
    //val c = nc.counter.toInt
    //nc.counter += 1
    //TypeVariable(charRange(c % charRange.length.toInt).toChar.toString + s"_$c", trace)
  //}
    

  //type Substitution = Map[TypeVariable, Type]
  //def substitute(t: Type, subst: Substitution): Type =
    //t match {
      //case tv: TypeVariable => subst.getOrElse(tv, tv)
      //case TypeArrow(l, r) => TypeArrow(substitute(l, subst), substitute(r, subst))
      //case TypeConstructor(name, p) => TypeConstructor(name, p.map(x => substitute(x, subst)))
    //}

  //def substitute(p: Poly, subst: Substitution): Poly =
    //Poly(p.boundTypes, substitute(p.t, subst -- p.boundTypes))

  //def substitute(env: Environment, subst: Substitution): Environment =
    //env.substituteInEnv(subst)

  //def freeType(t: Type): Set[TypeVariable] = 
    //t match {
      //case tv: TypeVariable => Set(tv)
      //case TypeArrow(l, r) => freeType(l) union freeType(r)
      //case TypeConstructor(_, p) => p.toSet.flatMap(freeType)
    //}

  //def freePoly(p: Poly): Set[TypeVariable] =
    //freeType(p.t) -- p.boundTypes

  //def freeEnv(env: Environment): Set[TypeVariable] = env.freeInEnv

  //def inst(p: Poly)(implicit nc: NamingContext): Poly = {
    //val freshBound = p.boundTypes.map(b => b -> fresh(trace=b.trace)).toMap
    //val subType = substitute(p.t, freshBound)
    //Poly(freshBound.values.toSet, subType)
  //}
  
  //class Environment(inner: Map[ProgramVariable, Poly]) {
    //def freeInEnv: Set[TypeVariable] = inner.values.toSet.flatMap(freePoly)

    //def substituteInEnv(subst: Substitution): Environment = new Environment(inner.map{ case (k, v) => k -> substitute(v, subst) })

    //def extend(pv: ProgramVariable, p: Poly) = new Environment(inner + (pv -> p))

    //def get(pv: ProgramVariable): Poly = 
      //inner.getOrElse(pv, throw new Exceptino(s"$pv is not bound"))

    //def gen(t: Type) = 
      //Poly(freeType(t) -- freeInEnv, t)

    //override def toString: String = 
      //inner.map{ case (k, v) =>
        //s"$k -> $v"
      //}.mkString("\n")
  //}
  //object Environment {
    //def apply: Environment = new Environment(Map.empty)
  //}

  //def combine(s1: Substitution, s2: Substitution): Substitution =
    //s2.mapValues(v => substitute(v, s1)) ++ s1
  
  //def unify(t1: Type, t2: Type): Substitution = (t1, t2) match {
    //case (TypeArrow(l1, r1), TypeArrow(l2, r2)) =>
      //val s1 = unify(l1, l2)
      //val s2 = unify(substitute(r1, s1), substitute(r2, s1))
      //combine(s2, s1)
    //case (TypeVariable(v1, _), TypeVariable(v2, _)) => Map.empty
    //case (tv@TypeVariable(_, _), r) => varSubst(tv, r)
    //case (l, tv@TypeVariable(_, _)) => varSubst(tv, l)
    //case (TypeConstructor(n1, ps1), TypeConstructor(n2, ps2)) if n1 == n2 =>
      //val initial: Substitution = Map.empty
      //val comb: List[(Type, Type)] = ps1 zip ps2
      //comb.foldLeft(initial){ case (subst, (p1, p2)) =>
        //val ns: Substitution = unify(substitute(p1, subst), substitute(p2, subst))
        //combine(ns, subst)
      //}
    //case (l, r) =>
      //throw new Exception(s"failed to unify $l and $r")
  //}

  //def varSubst(v: TypeVariable, t: Type) = 
    //if (freeType(t).map(_.name).contains(v.name)) {
      //throw new Exception(s"cannot unify $t and $v")
    //} else {
      //Map(v -> t)
    //}

  //def addMeta(fm: FunctionMeta, env: Environment)(implicit nc: NamingContext): Environment = {
    //val nt: Poly = fm.tpe match {
      //case None => 
        //val nv = fresh(trace=s"inferred for ${fm.name}")
        //Poly(Set(nv), nv)
      //case Some(t) => 
        //convertPoly(t)
    //}
    //env.extend(ProgramVariable(fm.name), nt)
  //}

  //def inferExpr(e: Expression, env: Environment, subst: Substitution)(implicit nc: NamingContext): (Substitution, Type) = e match {
    //case Var(name) => env.get(ProgramVariable(name))
    //case App(l, r) =>
      //val freshOut = fresh(r.toString)
      //val (s1, t1) = inferExpr(l, env)
      //val (s2, t2) = inferExpr(r, substitute(env, s1))
      //val s3 = unify(substitute(s2, t1), TypeArrow(t2, freshOut))
      //combine(combine(s3, s2), s1) -> freshOut
  //}

  //def bindName(env: Environment, nbm: NameBindingMatch)(implicit nc: NamingContext) = {
    //val f = fresh(trace=nbm.binding)
    //(env.extend(ProgramVariable(binding), Poly(Set.empty, f)), f)
  //}

  //def inferMatchType(products: Map[String, ProductType], env: Environment, matches: List[Match])(implicit nc: NamingContext): (Environment, Type) = (matches, t) match {    
    //case Nil => env
    //case x :: xs =>
      //x match {
        //case nmb:NameBindingMatch => bindName(env, nbm)
        //case TypeConstructorMatch(cname, bindings) => 
          //val product = products.getOrElse(cname, throw new Exception(s"could not find value constructor for $cname"))
          //val productTypes = product.typevars
          //val sumType = env.get(cname)
          //val ne = (bindings zip productTypes).foldLeft(env){ case (pe, (bindingName, productType)) =>
            //val asPoly: Poly = convertPoly(productType)
            //e.extend(ProgramVariable(bindingName.binding), asPoly)
          //}
      //}
  //}

  //def checkFs(fgs: List[FunctionGroup], env: Environment)(implicit nc: NamingContext) = {
    ////add function meta types first since polymorphic recursion is evil
    //val envWithMetas = fgs.map(_.meta).foldLeft(env){ case (accum, next) =>
      //addMeta(next, accum)
    //}

    //val emptySubst: Substitution = Map.empty
    //val checkedBodies = fgs.foldLeft(emptySubst){ case (accum, FunctionGroup(meta, impls)) =>
      //impls.foldLeft((accum, fresh(trace=s"tv for ${meta.name}"))){ case ((subst, curType), nextImpl) =>

        //checkExpr(nextImpl.expr, envWithMetas, subst)
      //}
    //}

    //envWithMetas
  //}
    

  //def check(decls: List[Either[SumType, FunctionGroup]]) = {
    //implicit val nc = NamingContext(1)
    //val sums = decls.collect{ case Left(st) => st }
    //val fgs = decls.collect{ case Right(st) => st }
    //val e2 = checkFs(fgs, Environment.apply)
    //println(e2)
  //}
}
