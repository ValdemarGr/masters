package ir

import par.TokenTypes._

object LCLanguage {
  trait LCExp {
    def stringify: String
  }

  /*
     func y x = x + 1

     func f a = a + 2

     func main =
         let b = 44
         func g c = c + (f b)
         (g 1) + (y 2)
     ...
     toplevel =
       (λy.
         (λf.
           //main
           (λb.
             (λg.
                (+) (g 1) (y 2)
             ) (λc. (+) c (f b))
           ) (44)
         ) (λa. a + 1) /// f
       ) (λx.x + 1) /// y
     ------
     func f a = g a
     func g a = f a
     func main =
       f 2
     ....
     variant 1
     (λf.
       (λg.
         //main
         f f g 2
       ) (λg.λf.(λa.f f a))
     ) (λf.λg.(λa.g g a))
     why it doesnt work
     func f a = g a
     func g a = f a
     func main =
       let g = 42
       f 2
     ...
     (λf.
       (λg.
         (λg.
           //main
           f f g 2
         ) (42)
       ) (λg.λf.(λa.f f a))
     ) (λf.λg.(λa.g g a))
     ...
     variant 2
     (λf'.
       (λg'.
         (λf.
           (λg.
             //main
             f (2)
           ) (g' g' f')
         ) (f' f' g')
       ) (λg.λf.(λa.f f g a))
     ) (λf.λg.(λa.g g f a))
  */

  case class LCName(name: String) extends LCExp {
    def stringify: String = name
  }
  case class LCFunction(metaName: String, name: LCName, exp: LCExp) extends LCExp {
    def stringify: String = s"(λ ${name.stringify}.${exp.stringify})"
  }
  case class LCApplication(fst: LCExp, snd: LCExp) extends LCExp {
    def stringify: String = s"(${fst.stringify} ${snd.stringify})"
  }
  case class LCTerminalOperation(lh: LCExp, op: BuiltinOperator, rh: LCExp) extends LCExp {
    def stringify: String = s"${lh.stringify} ${op} ${rh.stringify}"
  }
  case class LCString(v: String) extends LCExp {
    def stringify: String = s"'${v}'"
  }
  case class LCNumber(v: Int) extends LCExp {
    def stringify: String = s"${v}"
  }
  case class LCRawCode(s: String) extends LCExp {
    def stringify: String = s"code ${s}"
  }
}

