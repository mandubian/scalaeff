package effects
package stdio

import shapeless._


sealed trait StdIO extends Effect

case class PutStr(s: String) extends StdIO {
  type T = Unit
  type ResI = Unit
  type ResO = Unit

  def handle[M[_], X](res: Unit)(k: Unit => Unit => M[X]): M[X] = k(print(s))()
}

case class PutChar(c: Char) extends StdIO {
  type T = Unit
  type ResI = Unit
  type ResO = Unit

  def handle[M[_], X](res: Unit)(k: Unit => Unit => M[X]): M[X] = k(print(c))()
}

case object GetStr extends StdIO {
  type T = String
  type ResI = Unit
  type ResO = Unit

  def handle[M[_], X](res: Unit)(k: String => Unit => M[X]): M[X] = k(scala.io.StdIn.readLine())()
}

case object GetChar extends StdIO {
  type T = Char
  type ResI = Unit
  type ResO = Unit

  def handle[M[_], X](res: Unit)(k: Char => Unit => M[X]): M[X] = k(scala.io.StdIn.readChar())()

}

object StdIO {

  def putStr[M[_], ES <: HList](s: String)(implicit prf: EffElem[StdIO, Unit, Unit, ES, ES]): EffM[M, Unit, ES, ES] =
    EffM.call[M, StdIO, ES, ES](PutStr(s))

  def putStrLn[M[_], ES <: HList](s: String)(implicit prf: EffElem[StdIO, Unit, Unit, ES, ES]): EffM[M, Unit, ES, ES] =
    EffM.call[M, StdIO, ES, ES](PutStr(s + "\n"))

  def putChar[M[_], ES <: HList](c: Char)(implicit prf: EffElem[StdIO, Unit, Unit, ES, ES]): EffM[M, Unit, ES, ES] =
    EffM.call[M, StdIO, ES, ES](PutChar(c))

  def putCharLn[M[_], ES <: HList](c: Char)(implicit prf: EffElem[StdIO, Unit, Unit, ES, ES]): EffM[M, Unit, ES, ES] =
    EffM.call[M, StdIO, ES, ES](PutStr(c + "\n"))

  def getStr[M[_], ES <: HList](implicit prf: EffElem[StdIO, Unit, Unit, ES, ES]): EffM[M, String, ES, ES] =
    EffM.call[M, StdIO, ES, ES](GetStr)

  def getChar[M[_], ES <: HList](implicit prf: EffElem[StdIO, Unit, Unit, ES, ES]): EffM[M, Char, ES, ES] =
    EffM.call[M, StdIO, ES, ES](GetChar)

  def print[M[_], A, ES <: HList](a: A)(implicit prf: EffElem[StdIO, Unit, Unit, ES, ES]): EffM[M, Unit, ES, ES] =
    putStr(a.toString)

  def println[M[_], A, ES <: HList](a: A)(implicit prf: EffElem[StdIO, Unit, Unit, ES, ES]): EffM[M, Unit, ES, ES] =
    putStrLn(a.toString)

}