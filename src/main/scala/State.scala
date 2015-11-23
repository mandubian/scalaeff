package effects

import shapeless.HList


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

// object State {
//   def get[M[_], A, ES <: HList](implicit prf: EffElem[State, A, A, ES, ES]): EffM[M, A, ES, ES] = EffM.call[M, State, ES, ES](Get[A]())
//   def put[M[_], A, ES <: HList](a: A)(implicit prf: EffElem[State, A, A, ES, ES]): EffM[M, Unit, ES, ES] = EffM.call[M, State, ES, ES](Put[A, A](a))

//   def update[M[_], A, ES <: HList](f: A => A)(implicit prf: EffElem[State, A, A, ES, ES]): EffM[M, Unit, ES, ES] =
//     for {
//       v <- get
//       u <- put(f(v))
//     } yield u

// }

case class StateEnv[M[_], ES <: HList](env: Env[M, ES]) {
  def get[A](implicit prf: EffElem[State, A, A, ES, ES]): EffM[M, A, ES, ES] = EffM.call[M, State, ES, ES](Get[A]())

  def put[A](a: A)(implicit prf: EffElem[State, A, A, ES, ES]): EffM[M, Unit, ES, ES] = EffM.call[M, State, ES, ES](Put[A, A](a))

  def update[A](f: A => A)(implicit prf: EffElem[State, A, A, ES, ES]): EffM[M, Unit, ES, ES] =
    for {
      v <- get
      u <- put(f(v))
    } yield u
}

/*
object State {

  implicit def handlerGet[M[_], A] = new Handler[Get[A], M] {
    def handle[X](eff: Get[A])(a: A)(k: A => A => M[X]): M[X] = k(a)(a)
  }

  implicit def handlerPut[M[_], A, B] = new Handler[Put[A, B], M] {
    def handle[X](eff: Put[A, B])(a: A)(k: Unit => B => M[X]): M[X] = k(())(eff.b)
  }

}

import shapeless._


class StateGen() {
  // type a
  // type b
  type T[A]       = A       :+: Unit      :+: CNil
  type ResI[A]    = A       :+: A         :+: CNil
  type ResO[A, B] = A       :+: B         :+: CNil
  type S[A, B]    = Get[A]  :+: Put[A, B] :+: CNil

  implicit def hget[M[_], A] = implicitly[Handler[Get[A], M]]
  implicit def hput[M[_], A, B] = implicitly[Handler[Put[A, B], M]]

  def handle[M[_], X, A, B](eff: S[A, B])(resI: ResI[A])(k: T[A] => ResO[A, B] => M[X]): M[X] = eff match {
    case Inl(get) => hget[M, A].handle(get)(resI.head.get)(t => reso => k(Coproduct[T[A]](t))(Coproduct[ResO[A, B]](reso)))
    case Inr(Inl(put)) => hput[M, A, B].handle(put)(resI.tail.get.head.get)(t => reso => k(Coproduct[T[A]](t))(Coproduct[ResO[A, B]](reso)))
  }

  def lift[A, B](s: State): S[A, B] = s match {
    case g@Get() => Coproduct[S[A, B]](Get[A]())
    case p@Put(b) => Coproduct[S[A, B]](Put[A, B](b.asInstanceOf[B]))
  }
}

object StateGen {

  implicit def handlerState[M[_]] = new Handler[State, M] {
    def handle[X](eff: State)(res: eff.ResI)(k: eff.T => eff.ResO => M[X]): M[X] = {
      val st = new StateGen()
      st.handle(st.lift[eff.ResI, eff.ResO](eff))(Coproduct[st.ResI[eff.ResI]](res)) { st.T[eff.ResI] => st.ResO[eff.ResI,eff.ResO] =>

      }
    }
  }

}
*/