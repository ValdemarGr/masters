package runtime

import ir.LCLanguage._
import par.TokenTypes._

object ReductionMachine {
  type ReductionMap = Map[Identifier, LCExp]
  type Distance = List[Identifier]

  implicit final class Rmap(rm: ReductionMap) {
    def getErr(id: Identifier) = rm.getOrElse(id, throw new Exception(s"failed to find $id in $rm"))
  }

  def expectNum(exp: LCExp): Int = exp match {
    case LCNumber(v) => v
    case x => throw new Exception(s"looking for number, found $x")
  }

  def run(e: LCExp)(implicit reductionMap: ReductionMap, dist: Distance): LCExp = e match {
    case LCName(name) => run(reductionMap.getErr(name))
    case LCApplication(fst, snd) => run(snd)
    case LCTerminalOperation(lh, op, rh) => 
      val fst = expectNum(run(lh)) 
      val snd = expectNum(run(rh))
      LCNumber(fst + snd)
    case x => x
  }
}
