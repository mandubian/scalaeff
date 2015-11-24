package effects

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

  type MkState[R] = MkEff[State, R]

  def get[M[_], A, ES <: HList](implicit prf: EffElem[State, A, A, ES, ES]): EffM[M, A, ES, ES] = EffM.call[M, State, ES, ES](Get[A]())

  def put[M[_], A, ES <: HList](a: A)(implicit prf: EffElem[State, A, A, ES, ES]): EffM[M, Unit, ES, ES] = EffM.call[M, State, ES, ES](Put[A, A](a))

  def update[M[_], A, ES <: HList](f: A => A)(implicit prf: EffElem[State, A, A, ES, ES]): EffM[M, Unit, ES, ES] =
    for {
      v <- get
      u <- put(f(v))
    } yield u

  def putM[M[_], A, B, ES <: HList, ESO <: HList](b: B)(implicit prf: EffElem[State, A, B, ES, ESO]): EffM[M, Unit, ES, ESO] =
    EffM.call[M, State, ES, ESO](Put[A, B](b))


  def updateM[M[_], A, B, ES <: HList, ESO <: HList](f: A => B)(implicit prf: EffElem[State, A, A, ES, ES], prf2: EffElem[State, A, B, ES, ESO]): EffM[M, Unit, ES, ESO] =
    for {
      v <- get[M, A, ES]
      u <- putM[M, A, B, ES, ESO](f(v))
    } yield u
}

case class StateEnv[M[_], ES0 <: HList](env: Env[M, ES0]) {
  type ES = ES0

  def get[A](implicit prf: EffElem[State, A, A, ES, ES]) = State.get[M, A, ES]
  def getM[A, ES1 <: HList](implicit prf: EffElem[State, A, A, ES1, ES1]) = State.get[M, A, ES1]

  def put[A](a: A)(implicit prf: EffElem[State, A, A, ES, ES]) = State.put[M, A, ES](a)

  def update[A](f: A => A)(implicit prf: EffElem[State, A, A, ES, ES]) = State.update[M, A, ES](f)

  def putM[A, B, ESO <: HList](b: B)(implicit prf: EffElem[State, A, B, ES, ESO]) =
    State.putM[M, A, B, ES, ESO](b)

  def updateM[A, B, ESO <: HList](f: A => B)(implicit prf: EffElem[State, A, A, ES, ES], prf2: EffElem[State, A, B, ES, ESO]) =
    State.updateM[M, A, B, ES, ESO](f)

}

case class StateEnv0[M[_]]() {

  def get[A, ES <: HList](implicit prf: EffElem[State, A, A, ES, ES]) = State.get[M, A, ES]

  def put[A, ES <: HList](a: A)(implicit prf: EffElem[State, A, A, ES, ES]) = State.put[M, A, ES](a)

  def update[A, ES <: HList](f: A => A)(implicit prf: EffElem[State, A, A, ES, ES]) =
    State.update[M, A, ES](f)

  def putM[A, B, ES <: HList, ESO <: HList](b: B)(implicit prf: EffElem[State, A, B, ES, ESO]) =
    State.putM[M, A, B, ES, ESO](b)


  def updateM[A, B, ES <: HList, ESO <: HList](f: A => B)(implicit prf: EffElem[State, A, A, ES, ES], prf2: EffElem[State, A, B, ES, ESO]) =
    State.updateM[M, A, B, ES, ESO](f)

}
