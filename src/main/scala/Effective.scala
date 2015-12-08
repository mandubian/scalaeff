package effects

import shapeless.HList

trait Effective0[M0[_]] {

  implicit val ctx: Ctx.Aux[M0] = new Ctx { type M[a] = M0[a] }
  
  def apply[A, ESI <: HList, ESO <: HList](f: Ctx.Aux[M0] => EffM[M0, A, ESI, ESO]): EffM[M0, A, ESI, ESO] = f(ctx)

}


trait Effective[M0[_], ES <: HList] {

  implicit val ctx: Ctx.Aux[M0] = new Ctx { type M[a] = M0[a] }
  
  def apply[A, ESI <: HList, ESO <: HList](f: Ctx.Aux[M0] => EffM[M0, A, ESI, ESO])(
    implicit iso: IsoList[ESI, ES]
  ): EffM[M0, A, ESI, ESO] = f(ctx)

}

trait Effectives {
  def effective[M[_], ES <: HList]: Effective[M, ES] = new Effective[M, ES] {}

  def effective[M[_]]: Effective0[M] = new Effective0[M] {}
}
