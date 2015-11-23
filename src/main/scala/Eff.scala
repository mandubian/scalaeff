package effects

import shapeless._
import syntax.singleton._
import record._
import ops.hlist._
import syntax.SingletonOps

import cats.Applicative
import scala.language.higherKinds
import scala.language.experimental.macros

/** Effect Algebraic base */
trait Effect {
  type T
  type ResI
  type ResO

  def handle[M[_], A](res: ResI)(k: T => ResO => M[A]): M[A]
}

/** EFFECT reification with a resource */
trait EffectM
case class MkEff[E <: Effect, Res](res: Res) extends EffectM {

  // def -:[S <: AnyRef](s: S)(implicit w: Witness.Aux[s.type]): MkEff[E, LRes[w.T, Res]] = {
  //   MkEff[E, LRes[w.T, Res]](LRes(w.value, res))
  // }

}

// object MkEff {
//   def apply[E <: Effect, Res <: Tagged[_]](res: Res) = new MkEff[E, Res](tag[Res](res))
// }

trait Env[M[_], ES0 <: HList] {
  type ES = ES0
  type Eff[A] = EffM[M, A, ES, ES]

  def call[E <: Effect](eff: E)(implicit effElem: EffElem[E, eff.ResI, eff.ResO, ES, ES]) = EffM.call[M, E, ES, ES](eff)

}

object Env {
  def apply[M[_], ES <: HList] = new Env[M, ES] {}
}

// object Env {
//   def apply[M[_]] = new Env[M] {}
// }

case class LRes[Label, Res](lbl: Label, res: Res)

sealed trait EffM[M[_], A, ESI <: HList, ESO <: HList] {
  import EffM._

  def run(es: ESI)(implicit ap: Applicative[M]): M[A] = eff(es)(a => eso => ap.pure(a))
  
  def runPure(es: ESI)(implicit ev: M[A] =:= A, ap: Applicative[M]): M[(ESO, A)] =
      eff(es)( a => env => ap.pure((env, a)) )

  def eff[B](es: ESI)(ce: A => ESO => M[B]): M[B] = this match {
    case Value(a) => ce(a)(es.asInstanceOf[ESO])
    case EMap(effP, f) => effP.eff(es)(a => eso => ce(f(a))(eso))
    case EBind(effP, f) => effP.eff(es)(a => eso => f(a).eff(eso)(ce))
    case LiftP(effP, dropE, rebuildE) => effP.eff(dropE.drop(es))(a => eso => ce(a)(rebuildE.rebuild(eso, es)))
    case n: New[M, A, r0, e0, es] => n.effP.eff(MkEff[e0, r0](n.res) :: es)(a => eso => ce(a)(eso.asInstanceOf[ESO]))
    case c: CallP[M, t, e, es, eso] => c.execEff(es)(a => eso => ce(a)(eso))
  }

  def map[B, ESO2 <: HList](f: A => B): EffM[M, B, ESI, ESO] = EMap[M, A, B, ESI, ESO](this, f)

  def flatMap[B, ESO2 <: HList](f: A => EffM[M, B, ESO, ESO2]): EffM[M, B, ESI, ESO2] = EBind[M, A, B, ESI, ESO, ESO2](this, f)

  // def lift[Super <: HList](
  //   implicit dropE: DropE[ESI, Super], rebuildE: RebuildE[ESO, ESI, Super]
  // ): EffM[M, A, Super, ESO] =
  //   LiftP[M, A, Super, ESI, ESO](this, dropE, rebuildE)

  // def -:[S <: AnyRef](s: S)(implicit w: Witness.Aux[s.type]): EffM[M, A, Super, ESO] = {
  //   // val w = s.witness
  //   MkEff[E, LRes[w.T, Res]](LRes(w.value, res))
  // }
}


object EffM {
  case class Value[M[_], ES <: HList, A](a: A) extends EffM[M, A, ES, ES] {
    type T = A
  }

  case class New[M[_], A, R, E <: Effect, ES <: HList](
    res: R, e: MkEff[E, R], effP: EffM[M, A, MkEff[E, R] :: ES, MkEff[E, R] :: ES]
  ) extends EffM[M, A, ES, ES] {}

  case class LiftP[M[_], A, XS <: HList, YS <: HList, YSO <: HList](
      effP: EffM[M, A, YS, YSO]
    , dropE: DropE[YS, XS]
    , rebuildE: RebuildE[YSO, YS, XS]
  ) extends EffM[M, A, XS, YSO] {
    type _Sub = YS
  }

  abstract class CallP[M[_], T, E <: Effect, ES <: HList, ESO <: HList](
    val eff: E
  ) extends EffM[M, T, ES, ESO] {

    val prf: EffElem[E, eff.ResI, eff.ResO, ES, ESO]

    def execEff[B](es: ES)(ce: T => ESO => M[B]): M[B] = {
      val m: MkEff[E, eff.ResI] = prf.sel(es)
      val res: eff.ResI = m.res
      eff.handle(res) { (a:eff.T) => (resO: eff.ResO) =>
        val eso = prf.rep.apply(es, MkEff[E, eff.ResO](resO))
        ce(a.asInstanceOf[T])(eso)
      }
    }

  }

  case class EBind[M[_], A, B, ES <: HList, ESO <: HList, ESO2 <: HList](
    effP: EffM[M, A, ES, ESO], f: A => EffM[M, B, ESO, ESO2]
  ) extends EffM[M, B, ES, ESO2] {
    type _ESO = ESO
  }

  case class EMap[M[_], A, B, ES <: HList, ESO <: HList](
    effP: EffM[M, A, ES, ESO], f: A => B
  ) extends EffM[M, B, ES, ESO]

  case class Labelled[Label, M[_], A, E <: Effect, Res, ESI <: HList, ESO <: HList](
    lbl: Label,
    effP: EffM[M, A, MkEff[E, Res] :: ESI, ESO]
  ) extends EffM[M, A, MkEff[E, LRes[Label, Res]] :: ESI, ESO]

  def pure[M[_], ES <: HList, A](a: A): EffM[M, A, ES, ES] = Value(a)

  def call[M[_], E <: Effect, ES <: HList, ESO <: HList](eff: E)(
    implicit effElem: EffElem[E, eff.ResI, eff.ResO, ES, ESO] //sel: EnvSelector[M, ES, E, eff.ResI], upd: EnvUpdater[M, E, ES, ESO, eff.ResI, eff.ResO]
  ): EffM[M, eff.T, ES, ESO] = new CallP[M, eff.T, E, ES, ESO](eff) {
    val prf = effElem.asInstanceOf[EffElem[E, eff.ResI, eff.ResO, ES, ESO]] // WHY IS IT NEEDED SCALAC????
  }


}

case class Eff[M[_], ES <: HList]() {
  def call[E <: Effect](eff: E)(implicit effElem: EffElem[E, eff.ResI, eff.ResO, ES, ES]) = EffM.call[M, E, ES, ES](eff)
}

case class EffElem[E <: Effect, Res, ResO, ES <: HList, ESO <: HList](
  sel: Selector[ES, MkEff[E, Res]],
  rep: Rep.Aux[ES, MkEff[E, Res], MkEff[E, ResO], ESO]
) 

object EffElem {
  implicit def mkEffElem[M[_], E <: Effect, Res, ResO, ES <: HList, ESO <: HList]
    (implicit
      sel: Selector[ES, MkEff[E, Res]]
    , upd: Rep.Aux[ES, MkEff[E, Res], MkEff[E, ResO], ESO]
    ) =
      EffElem[E, Res, ResO, ES, ESO](sel, upd)

}


trait Rep[L <: HList, U, V] {
  type Out <: HList
  def apply(l: L, v: V): Out
}

object Rep {
  type Aux[L <: HList, U, V, Out0 <: HList] = Rep[L, U, V] { type Out = Out0 }
  implicit def rep[L <: HList, U, V, Out0 <: HList](implicit rep: Replacer.Aux[L, U, V, (U, Out0)]): Aux[L, U, V, Out0] =
    new Rep[L, U, V] {
      type Out = Out0
      def apply(l: L, v: V): Out = rep(l, v)._2
    }
}

trait DropE[Small <: HList, Big <: HList] {
  def drop(e: Big): Small
}

object DropE {
  implicit object nil extends DropE[HNil, HNil] {
    def drop(e: HNil): HNil = HNil
  }

  implicit def keep[H, HL1 <: HList, HL2 <: HList](implicit de: DropE[HL1, HL2]) = new DropE[H :: HL1, H :: HL2] {
    def drop(e: H :: HL2): H :: HL1 = e.head :: de.drop(e.tail)
  }

  implicit def drop[HL1 <: HList, H, HL2 <: HList](implicit de: DropE[HL1, HL2]) = new DropE[HL1, H :: HL2] {
    def drop(e: H :: HL2): HL1 = de.drop(e.tail)
  }
}


trait RebuildE[Ref <: HList, Small <: HList, Big <: HList] {
  def rebuild(es1: Ref, es2: Big): Ref
}

object RebuildE extends RebuildE2 {

  implicit object nil extends RebuildE[HNil, HNil, HNil] {
    def rebuild(es1: HNil, es2: HNil): HNil = HNil
  }

}

trait RebuildE2 extends RebuildE3 {

  implicit def rightNil[H, XS <: HList] = new RebuildE[H :: XS, HNil, HNil] {
    def rebuild(es1: H :: XS, es2: HNil): H :: XS = es1
  }

}

trait RebuildE3 extends RebuildE4 {

  implicit def leftNil[XH, XS <: HList] = new RebuildE[HNil, XH :: XS, XH :: XS] {
    def rebuild(es1: HNil, es2: XH :: XS): HNil = es1
  }

  implicit def keepLeft[XH, XS <: HList, YH, YS <: HList](implicit sub: RebuildE[YS, XS, XS]) =
    new RebuildE[YH :: YS , XH :: XS , XH :: XS] {
      def rebuild(es1: YH :: YS, es2: XH :: XS): YH :: YS = es1.head :: sub.rebuild(es1.tail, es2.tail)
    }

}

trait RebuildE4 {

  implicit def dropRight[XH, XS <: HList, YS <: HList](implicit sub: RebuildE[YS, XS, XS]) =
    new RebuildE[YS, XH :: XS, XH :: XS] {
      def rebuild(es1: YS, es2: XH :: XS): YS = sub.rebuild(es1, es2.tail)
    }

}



