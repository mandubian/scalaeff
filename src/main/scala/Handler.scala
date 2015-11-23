package eff

import shapeless._
/*
trait Handler[E <: Effect, M[_]] {
  def handle[A](eff: E)(res: eff.ResI)(k: eff.T => eff.ResO => M[A]): M[A]
}

object Handler {
  implicit def t[E <: Effect, M[_], H <: Effect, R <: Coproduct](
    implicit gen: Generic.Aux[E, H :+: R], hash: Lazy[CopHandlers[M, H, R]]
  ) = new Handler[E, M] {
    def handle[A](eff: E)(res: eff.ResI)(k: eff.T => eff.ResO => M[A]): M[A] =
      hash.value.handle(gen.to(eff))(res)(k.asInstanceOf[Any => Any => M[A]])
  }
}


trait CopHandlers[M[_], SE <: Effect, R <: Coproduct] {
  val lh: Handler[SE, M]

  // Use Any as we don't know yet what is the local type
  // Could a Coproduct of all local types be used instead of Any too?
  def handle[A](eff: SE :+: R)(res: Any)(k: Any => Any => M[A]): M[A]

}


object CopHandlers {
  implicit def one[M[_], SE0 <: Effect](implicit h: Handler[SE0, M]) =
    new CopHandlers[M, SE0, CNil] {
      val lh = h

      def handle[A](eff: SE0 :+: CNil)(res:Any)(k: Any => Any => M[A]): M[A] = eff match {
        case Inl(se) => lh.handle(se)(res.asInstanceOf[se.ResI])(k.asInstanceOf[se.T => se.ResO => M[A]])
        case Inr(_) => throw new RuntimeException("impossible case")
      }

    }

  implicit def two[M[_], SE0 <: Effect, SE1 <: Effect, R <: Coproduct](
    implicit h: Handler[SE0, M], hash: CopHandlers[M, SE1, R]) =
    new CopHandlers[M, SE0, SE1 :+: R] {
      val lh = h

      def handle[A](eff: SE0 :+: SE1 :+: R)(res: Any)(k: Any => Any => M[A]): M[A] = eff match {
        case Inl(se) => lh.handle(se)(res.asInstanceOf[se.ResI])(k.asInstanceOf[se.T => se.ResO => M[A]])
        case Inr(r) => hash.handle(r)(res)(k)
      }

    }
}
*/

// trait EffectX[E <: Effect, T, ResI, ResO]

// object EffectX {
//   implicit def fx[E <: Effect] = new EffectX[E, E#T, E#ResI, E#ResO] {}
// }
