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

  def handle[M[_], X](a: A)(k: A => A => M[X]): M[X] = k(a)(a)
}

case class Put[A, B](b: B) extends State {
  type T = Unit
  type ResI = A
  type ResO = B

  def handle[M[_], X](a: A)(k: Unit => B => M[X]): M[X] = k(())(b)
}


object State {


  def get[M[_], A, ES <: HList](implicit prf: EffElem.Aux[State, A, A, ES, ES]): EffM[M, A, ES, ES] =
    EffM.call[M, State, ES](Get[A]())

  def get2[A, ES <: HList](implicit ctx: Ctx, prf: EffElem.Aux[State, A, A, ES, ES]): EffM[ctx.M, A, ES, ES] =
    EffM.call[ctx.M, State, ES](Get[A]())

  def get3[M[_], A]: EffM[M, A, MkEff[State, A] :: HNil, MkEff[State, A] :: HNil] =
    EffM.call[M, State, MkEff[State, A] :: HNil](Get[A]())

  def get4[A](implicit ctx: Ctx): EffM[ctx.M, A, MkEff[State, A] :: HNil, MkEff[State, A] :: HNil] =
    EffM.call[ctx.M, State, MkEff[State, A] :: HNil](Get[A]())

  def put[M[_], A, ES <: HList](a: A)(implicit prf: EffElem.Aux[State, A, A, ES, ES]): EffM[M, Unit, ES, ES] =
    EffM.call[M, State, ES](Put[A, A](a))

  def put3[M[_], A](a: A): EffM[M, Unit, MkEff[State, A] :: HNil, MkEff[State, A] :: HNil] =
    EffM.call[M, State, MkEff[State, A] :: HNil](Put[A, A](a))

  def put4[A](a: A)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[State, A] :: HNil, MkEff[State, A] :: HNil] =
    EffM.call[ctx.M, State, MkEff[State, A] :: HNil](Put[A, A](a))

  trait Labelled[L] {
    def get[A](implicit ctx: Ctx): EffM[ctx.M, A, MkEff[State@@L, A] :: HNil, MkEff[State@@L, A] :: HNil] =
      EffM.call[ctx.M, State@@L, MkEff[State@@L, A] :: HNil](Get[A]())

    def getM[M[_], A]: EffM[M, A, MkEff[State@@L, A] :: HNil, MkEff[State@@L, A] :: HNil] =
      EffM.call[M, State@@L, MkEff[State@@L, A] :: HNil](Get[A]())

    def put[A](a: A)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[State@@L, A] :: HNil, MkEff[State@@L, A] :: HNil] =
      EffM.call[ctx.M, State@@L, MkEff[State@@L, A] :: HNil](Put[A, A](a))

    def putM[M[_], A](a: A): EffM[M, Unit, MkEff[State@@L, A] :: HNil, MkEff[State@@L, A] :: HNil] =
      EffM.call[M, State@@L, MkEff[State@@L, A] :: HNil](Put[A, A](a))

    def putB[M[_], A, B](b: B): EffM[M, Unit, MkEff[State@@L, A] :: HNil, MkEff[State@@L, B] :: HNil] =
      EffM.call[M, State@@L, MkEff[State@@L, A] :: HNil](Put[A, B](b))
  }

  def apply[L] = new Labelled[L] {}
  // def update[M[_], A, ES <: HList](f: A => A)(implicit prf: EffElem.Aux[State, A, A, ES, ES]): EffM[M, Unit, ES, ES] =
  //   for {
  //     v <- get[M, A, ES]
  //     u <- put(f(v))
  //   } yield u

  // def putM[M[_], A, B, ES <: HList](b: B)(implicit prf: EffElem[State, A, B, ES]): EffM[M, Unit, ES, prf.ESO] =
  //   EffM.call[M, State, ES](Put[A, B](b))

  def putM[M[_], A, B](b: B): EffM[M, Unit, MkEff[State, A] :: HNil, MkEff[State, B] :: HNil] =
    EffM.call[M, State, MkEff[State, A] :: HNil](Put[A, B](b))


  // def updateM[M[_], A, B, ES <: HList](f: A => B)(
  //   implicit prf: EffElem.Aux[State, A, A, ES, ES], prf2: EffElem[State, A, B, ES]
  // ): EffM[M, Unit, ES, prf2.ESO] =
  //   for {
  //     v <- get[M, A, ES]
  //     u <- putM(f(v))
  //   } yield u
}
/*
case class StateEnv[M[_], ES0 <: HList](env: Env[M, ES0]) {
  type ES = ES0

  def get[A](implicit prf: EffElem.Aux[State, A, A, ES, ES]) = State.get[M, A, ES]
  def getM[A, ES1 <: HList](implicit prf: EffElem.Aux[State, A, A, ES1, ES1]) = State.get[M, A, ES1]

  def put[A](a: A)(implicit prf: EffElem.Aux[State, A, A, ES, ES]) = State.put[M, A, ES](a)

  def update[A](f: A => A)(implicit prf: EffElem.Aux[State, A, A, ES, ES]) = State.update[M, A, ES](f)

  def putM[A, B](b: B)(implicit prf: EffElem[State, A, B, ES]) =
    State.putM[M, A, B, ES](b)

  def updateM[A, B](f: A => B)(implicit prf: EffElem.Aux[State, A, A, ES, ES], prf2: EffElem[State, A, B, ES]) =
    State.updateM[M, A, B, ES](f)

}

case class StateEnv0[M[_]]() {

  def get[A, ES <: HList](implicit prf: EffElem.Aux[State, A, A, ES, ES]) = State.get[M, A, ES]

  def put[A, ES <: HList](a: A)(implicit prf: EffElem.Aux[State, A, A, ES, ES]) = State.put[M, A, ES](a)

  def update[A, ES <: HList](f: A => A)(implicit prf: EffElem.Aux[State, A, A, ES, ES]) =
    State.update[M, A, ES](f)

  def putM[A, B, ES <: HList](b: B)(implicit prf: EffElem[State, A, B, ES]) =
    State.putM[M, A, B, ES](b)

  def updateM[A, B, ES <: HList](f: A => B)(
    implicit prf: EffElem.Aux[State, A, A, ES, ES], prf2: EffElem[State, A, B, ES]
  ) =
    State.updateM[M, A, B, ES](f)

}
*/