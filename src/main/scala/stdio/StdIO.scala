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

  // def handle[M[_], X](res: Unit)(k: Unit => Unit => M[X]): M[X] = k(print(s))()
}

case class PutChar(c: Char) extends StdIO {
  type T = Unit
  type ResI = Unit
  type ResO = Unit

  // def handle[M[_], X](res: Unit)(k: Unit => Unit => M[X]): M[X] = k(print(c))()
}

case object GetStr extends StdIO {
  type T = String
  type ResI = Unit
  type ResO = Unit

  // def handle[M[_], X](res: Unit)(k: String => Unit => M[X]): M[X] = k(scala.io.StdIn.readLine())()
}

case object GetChar extends StdIO {
  type T = Char
  type ResI = Unit
  type ResO = Unit

  // def handle[M[_], X](res: Unit)(k: Char => Unit => M[X]): M[X] = k(scala.io.StdIn.readChar())()

}

object StdIO extends EffectBuilder[StdIO] {

  // PUTSTR
  def putStr0[M[_]](s: String): EffM[M, Unit, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[PutStr, M] :: HNil] =
    EffM.call[M, StdIO, PutStr, MkEff[StdIO, Unit] :: HNil, Handler[PutStr, M] :: HNil](PutStr(s))

  def putStr(s: String)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[PutStr, ctx.M] :: HNil] =
    putStr0(s)

  def putStrLn0[M[_]](s: String): EffM[M, Unit, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[PutStr, M] :: HNil] =
    putStr0(s + "\n")

  def putStrLn(s: String)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[PutStr, ctx.M] :: HNil] =
    putStrLn0(s)

  // PUTCHAR
  def putChar0[M[_]](c: Char): EffM[M, Unit, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[PutChar, M] :: HNil] =
    EffM.call[M, StdIO, PutChar, MkEff[StdIO, Unit] :: HNil, Handler[PutChar, M] :: HNil](PutChar(c))

  def putChar(c: Char)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[PutChar, ctx.M] :: HNil] =
    putChar(c)

  def putCharLn0[M[_]](c: Char): EffM[M, Unit, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[PutStr, M] :: HNil] =
    putStr0(c + "\n")

  def putCharLn(c: Char)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[PutStr, ctx.M] :: HNil] =
    putCharLn0(c)

  // GETSTR
  def getStr0[M[_]](): EffM[M, String, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[GetStr.type, M] :: HNil] =
    EffM.call[M, StdIO, GetStr.type, MkEff[StdIO, Unit] :: HNil, Handler[GetStr.type, M] :: HNil](GetStr)

  def getStr()(implicit ctx: Ctx): EffM[ctx.M, String, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[GetStr.type, ctx.M] :: HNil] =
    getStr0()

  // GETCHAR
  def getChar0[M[_]](): EffM[M, Char, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[GetChar.type, M] :: HNil] =
    EffM.call[M, StdIO, GetChar.type, MkEff[StdIO, Unit] :: HNil, Handler[GetChar.type, M] :: HNil](GetChar)

  def getChar()(implicit ctx: Ctx): EffM[ctx.M, Char, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[GetChar.type, ctx.M] :: HNil] =
    getChar0()

  // PRINT
  def print0[M[_], A](a: A): EffM[M, Unit, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[PutStr, M] :: HNil] =
    putStr0(a.toString)

  def print[A](a: A)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[PutStr, ctx.M] :: HNil] =
    print0(a)

  def println0[M[_], A](a: A): EffM[M, Unit, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[PutStr, M] :: HNil] =
    putStrLn0(a.toString)

  def println[A](a: A)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[StdIO, Unit] :: HNil, MkEff[StdIO, Unit] :: HNil, Handler[PutStr, ctx.M] :: HNil] =
    println0(a)

}