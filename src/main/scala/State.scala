package eff


sealed trait State extends Effect

case class Get[A]() extends State {
  type T = A
  type ResI = A
  type ResO = A

  // def handle[M[_], X](a: A)(k: A => A => M[X]): M[X] = k(a)(a)
}

case class Put[A, B](b: B) extends State {
  type T = Unit
  type ResI = A
  type ResO = B

  // def handle[M[_], X](a: A)(k: Unit => B => M[X]): M[X] = k(())(b)
}

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
  type a
  type b
  type T =    a       :+: Unit      :+: CNil
  type ResI = a       :+: a         :+: CNil
  type ResO = a       :+: b         :+: CNil
  type S =    Get[a]  :+: Put[a, b] :+: CNil

  implicit def hget[M[_]] = implicitly[Handler[Get[a], M]]
  implicit def hput[M[_]] = implicitly[Handler[Put[a, b], M]]

  def handle[M[_], X](eff: S)(resI: ResI)(k: T => ResO => M[X]): M[X] = eff match {
    case Inl(get) => hget[M].handle(get)(resI.head.get)(t => reso => k(Coproduct[T](t))(Coproduct[ResO](reso)))
    case Inr(Inl(put)) => hput[M].handle(put)(resI.tail.get.head.get)(t => reso => k(Coproduct[T](t))(Coproduct[ResO](reso)))
  }

  def lift(s: State): S = s match {
    case g@Get() => Coproduct[S](Get[a]())
    case p@Put(b) => Coproduct[S](Put[a, b](b))
  }
}

object StateGen {

  implicit def handlerState[M[_]] = new Handler[State, M] {
    def handle[X](eff: State)(res: eff.ResI)(k: eff.T => eff.ResO => M[X]): M[X] =
      (new StateGen()).handle()
  }

}