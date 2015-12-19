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
package state

import shapeless.{HList, ::, HNil}


sealed trait State extends Effect

case class Get[A]() extends State {
  type T = A
  type ResI = A
  type ResO = A
}

case class Put[A, B](b: B) extends State {
  type T = Unit
  type ResI = A
  type ResO = B
}

object State extends EffectBuilder[State] {

  // GET
  def get0[M[_], A](): EffM[M, A, MkEff[State, A] :: HNil, MkEff[State, A] :: HNil, Handler[Get[A], M] :: HNil] =
    EffM.call[M, State, Get[A], MkEff[State, A] :: HNil, Handler[Get[A], M] :: HNil](Get[A]())

  def get[A](implicit ctx: Ctx): EffM[ctx.M, A, MkEff[State, A] :: HNil, MkEff[State, A] :: HNil, Handler[Get[A], ctx.M] :: HNil] =
    get0[ctx.M, A]

  // PUT
  def put0[M[_], A](a: A): EffM[M, Unit, MkEff[State, A] :: HNil, MkEff[State, A] :: HNil, Handler[Put[A, A], M] :: HNil] =
    EffM.call[M, State, Put[A, A], MkEff[State, A] :: HNil, Handler[Put[A, A], M] :: HNil](Put[A, A](a))

  def put[A](a: A)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[State, A] :: HNil, MkEff[State, A] :: HNil, Handler[Put[A, A], ctx.M] :: HNil] =
    put0[ctx.M, A](a)

  // UPDATE
  def update0[M[_], A](f: A => A): EffM[M, Unit, MkEff[State, A] :: HNil, MkEff[State, A] :: HNil, Handler[Get[A], M] :: Handler[Put[A, A], M] :: HNil] =
    for {
      v <- get0[M, A]
      u <- put0(f(v))
    } yield u


  def update[A](f: A => A)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[State, A] :: HNil, MkEff[State, A] :: HNil, Handler[Get[A], ctx.M] :: Handler[Put[A, A], ctx.M] :: HNil] =
    update0[ctx.M, A](f)


  // PUTM
  def putM0[M[_], A, B](b: B): EffM[M, Unit, MkEff[State, A] :: HNil, MkEff[State, B] :: HNil, Handler[Put[A, B], M] :: HNil] =
    EffM.call[M, State, Put[A, B], MkEff[State, A] :: HNil, Handler[Put[A, B], M] :: HNil](Put[A, B](b))

  def putM[A, B](b: B)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[State, A] :: HNil, MkEff[State, B] :: HNil, Handler[Put[A, B], ctx.M] :: HNil] =
    putM0[ctx.M, A, B](b)

  // UPDATEM
  def updateM0[M[_], A, B](f: A => B): EffM[M, Unit, MkEff[State, A] :: HNil, MkEff[State, B] :: HNil, Handler[Get[A], M] :: Handler[Put[A, B], M] :: HNil] =
    for {
      v <- get0[M, A]
      u <- putM0[M, A, B](f(v))
    } yield u

  def updateM[A, B](f: A => B)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[State, A] :: HNil, MkEff[State, B] :: HNil, Handler[Get[A], ctx.M] :: Handler[Put[A, B], ctx.M] :: HNil] =
    updateM0[ctx.M, A, B](f)

  // def apply[L] = new Labelled[L] {}

  // trait Labelled[L] {
  //   // GET
  //   def get0[M[_], A](): EffM[M, A, MkEff[State@@L, A] :: HNil, MkEff[State@@L, A] :: HNil, Handler[Get[A]@@L, M] :: HNil] =
  //     EffM.call[M, State@@L, Get[A]@@L, MkEff[State@@L, A] :: HNil, Handler[Get[A]@@L, M] :: HNil](Get[A]())

  //   def get[A](implicit ctx: Ctx): EffM[ctx.M, A, MkEff[State@@L, A] :: HNil, MkEff[State@@L, A] :: HNil, Handler[Get[A]@@L, ctx.M] :: HNil] =
  //     get0[ctx.M, A]

  //   // PUT
  //   def put0[M[_], A](a: A): EffM[M, Unit, MkEff[State@@L, A] :: HNil, MkEff[State@@L, A] :: HNil, Handler[Put[A, A]@@L, M] :: HNil] =
  //     EffM.call[M, State@@L, Put[A, A]@@L, MkEff[State@@L, A] :: HNil, Handler[Put[A, A]@@L, M] :: HNil](Put[A, A](a))

  //   def put[A](a: A)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[State@@L, A] :: HNil, MkEff[State@@L, A] :: HNil, Handler[Put[A, A]@@L, ctx.M] :: HNil] =
  //     put0[ctx.M, A](a)

  //   // UPDATE
  //   def update0[M[_], A](f: A => A): EffM[M, Unit, MkEff[State@@L, A] :: HNil, MkEff[State@@L, A] :: HNil, Handler[Get[A]@@L, M] :: Handler[Put[A, A]@@L, M] :: HNil] =
  //     for {
  //       v <- get0[M, A]
  //       u <- put0(f(v))
  //     } yield u


  //   def update[A](f: A => A)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[State@@L, A] :: HNil, MkEff[State@@L, A] :: HNil, Handler[Get[A]@@L, ctx.M] :: Handler[Put[A, A]@@L, ctx.M] :: HNil] =
  //     update0[ctx.M, A](f)


  //   // PUTM
  //   def putM0[M[_], A, B](b: B): EffM[M, Unit, MkEff[State@@L, A] :: HNil, MkEff[State@@L, B] :: HNil, Handler[Put[A, B]@@L, M] :: HNil] =
  //     EffM.call[M, State@@L, Put[A, B]@@L, MkEff[State@@L, A] :: HNil, Handler[Put[A, B]@@L, M] :: HNil](Put[A, B](b))

  //   def putM[A, B](b: B)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[State@@L, A] :: HNil, MkEff[State@@L, B] :: HNil, Handler[Put[A, B]@@L, ctx.M] :: HNil] =
  //     putM0[ctx.M, A, B](b)

  //   // UPDATEM
  //   def updateM0[M[_], A, B](f: A => B): EffM[M, Unit, MkEff[State@@L, A] :: HNil, MkEff[State@@L, B] :: HNil, Handler[Get[A]@@L, M] :: Handler[Put[A, B]@@L, M] :: HNil] =
  //     for {
  //       v <- get0[M, A]
  //       u <- putM0[M, A, B](f(v))
  //     } yield u

  //   def updateM[A, B](f: A => B)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[State@@L, A] :: HNil, MkEff[State@@L, B] :: HNil, Handler[Get[A]@@L, ctx.M] :: Handler[Put[A, B]@@L, ctx.M] :: HNil] =
  //     updateM0[ctx.M, A, B](f)

  // }

}
