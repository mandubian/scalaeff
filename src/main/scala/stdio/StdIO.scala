/*
Copyright 2015 Miles Sabin & Pascal Voitot

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
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