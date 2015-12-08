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

import shapeless._
import syntax.singleton._
import record._
import ops.hlist._
import syntax.SingletonOps

import cats.Applicative
import scala.language.higherKinds
import scala.language.experimental.macros

import scala.annotation.implicitNotFound


/** Effect Algebraic base */
trait Effect {
  type T
  type ResI
  type ResO

  def handle[M[_], A](res: ResI)(k: T => ResO => M[A]): M[A]
}


/** EFFECT reification with a resource */
trait EffectM
case class MkEff[E <: Effect, Res](res: Res) extends EffectM

trait Env[M[_], ES0 <: HList] {
  type ES = ES0

  type Eff[A] = EffM[M, A, ES, ES]

}

object Env {
  def apply[M[_], ES <: HList] = new Env[M, ES] {}
}

sealed trait EffM[M[_], A, ESI <: HList, ESO <: HList] {
  import EffM._

  def run[ESI0 <: HList](es: ESI0)(
    implicit ap: Applicative[M],
             iso: IsoList[ESI0, ESI]
  ): M[A] = eff(iso(es))(a => eso => ap.pure(a))
  
  def runPure(es: ESI)(implicit ev: M[A] =:= A, ap: Applicative[M]): M[(ESO, A)] =
      eff(es)( a => env => ap.pure((env, a)) )

  def eff[B](es: ESI)(ce: A => ESO => M[B]): M[B] = this match {
    case Value(a) => ce(a)(es.asInstanceOf[ESO])
    case EMap(effP, f) => effP.eff(es)(a => eso => ce(f(a))(eso))

    case EFlatMap(effP, f, prf) =>
          effP.eff(prf.downI(es)) { a => eso =>
            f(a).eff(prf.out2in(es, eso)) { a => eso2 =>
              ce(a)(prf.buildOut(eso, eso2))
            }
          }

    case EBind(effP, f) => effP.eff(es)(a => eso => f(a).eff(eso)(ce))
    case l@LiftP(effP, dropE, rebuildEO) => effP.eff(dropE.drop(es))(a => eso => ce(a)(rebuildEO.rebuild(es, eso)))
    case New(e, effP) => effP.eff(e :: es)(a => eso => ce(a)(eso.tail))
    case c: CallP[M, t, e, es, eso] => c.execEff(es)(a => eso => ce(a)(eso))
  }

  def map[B](f: A => B): EffM[M, B, ESI, ESO] = EMap[M, A, B, ESI, ESO](this, f)

  // def map[B, ESI2 <: HList, ESO2 <: HList](f: A => B)(
  //   implicit mappable: Mappable.Aux[ESI, ESO, ESI2, ESO2]
  // ): EffM[M, B, ESI2, ESO2] = EMap2[M, A, B, ESI, ESO, ESI2, ESO2](this, f, mappable)

  // def map[B, Super <: HList, SuperO <: HList](f: A => B)(
  //   implicit dropE: DropE[ESI, Super], rebuildEO: RebuildEO[Super, ESO, SuperO]
  // ): EffM[M, B, Super, SuperO] = EMap[M, A, B, ESI, ESO](this, f).lift[Super, SuperO]

  // def flatMap[B, ESO2 <: HList](f: A => EffM[M, B, ESO, ESO2]): EffM[M, B, ESI, ESO2] = EBind[M, A, B, ESI, ESO, ESO2](this, f)

  // def flatMap[B, ESO2 <: HList, Super <: HList, SuperO <: HList](f: A => EffM[M, B, ESO, ESO2])(
  //   implicit dropE: DropE[ESI, Super], rebuildEO: RebuildEO[Super, ESO2, SuperO]
  // ): EffM[M, B, Super, SuperO] = EBind[M, A, B, ESI, ESO, ESO2](this, f).lift2[Super, SuperO]

  def flatMap[B, ESI2 <: HList, ESO2 <: HList, ESI3 <: HList, ESO3 <: HList](f: A => EffM[M, B, ESI2, ESO2])(
    implicit flatMappable: FlatMappable.Aux[ESI, ESO, ESI2, ESO2, ESI3, ESO3]
  ): EffM[M, B, ESI3, ESO3] = EFlatMap(this, f, flatMappable)

  def lift2[Super <: HList, SuperO <: HList](
    implicit dropE: DropE[ESI, Super], rebuildEO: RebuildEO[Super, ESO, SuperO]
  ): EffM[M, A, Super, SuperO] =
    LiftP[M, A, ESI, ESO, Super, SuperO](this, dropE, rebuildEO)

  def lift[Super <: HList](
    implicit dropE: DropE[ESI, Super], rebuildEO: RebuildEO[Super, ESO, Super]
  ): EffM[M, A, Super, Super] =
    LiftP[M, A, ESI, ESO, Super, Super](this, dropE, rebuildEO)

}


object EffM {
  case class Value[M[_], ES <: HList, A](a: A) extends EffM[M, A, ES, ES] {
    type T = A
  }
 
  case class New[M[_], A, R, E <: Effect, ES <: HList, ESO <: HList](
    e: MkEff[E, R], effP: EffM[M, A, MkEff[E, R] :: ES, MkEff[E, R] :: ESO]
  ) extends EffM[M, A, ES, ESO] {}

  case class LiftP[M[_], A, ESI <: HList, ESO <: HList, ESI2 <: HList, ESO2 <: HList](
      effP: EffM[M, A, ESI, ESO]
    , dropE: DropE[ESI, ESI2]
    , rebuildEO: RebuildEO[ESI2, ESO, ESO2]
  ) extends EffM[M, A, ESI2, ESO2] {
    type _Sub = ESI
    type _SubO = ESO
  }

  abstract class CallP[M[_], T, E <: Effect, ES <: HList, ESO <: HList](
    val eff: E
  ) extends EffM[M, T, ES, ESO] {

    val prf: EffElem.Aux[E, eff.ResI, eff.ResO, ES, ESO]

    def execEff[B](es: ES)(ce: T => ESO => M[B]): M[B] = {
      val m: MkEff[E, eff.ResI] = prf.sel(es)
      val res: eff.ResI = m.res
      eff.handle(res) { (a:eff.T) => (resO: eff.ResO) =>
        val eso = prf.rep(es, MkEff[E, eff.ResO](resO))
        ce(a.asInstanceOf[T])(eso)
      }
    }

  }

  case class EFlatMap[M[_], A, B, ESI <: HList, ESO <: HList, ESI2 <: HList, ESO2 <: HList, ESI3 <: HList, ESO3 <: HList](
    effP: EffM[M, A, ESI, ESO]
  , f: A => EffM[M, B, ESI2, ESO2]
  , prf: FlatMappable.Aux[ESI, ESO, ESI2, ESO2, ESI3, ESO3]
  ) extends EffM[M, B, ESI3, ESO3]

  case class EBind[M[_], A, B, ES <: HList, ESO <: HList, ESO2 <: HList](
    effP: EffM[M, A, ES, ESO], f: A => EffM[M, B, ESO, ESO2]
  ) extends EffM[M, B, ES, ESO2] {
    type _ESO = ESO
  }

  case class EMap[M[_], A, B, ES <: HList, ESO <: HList](
    effP: EffM[M, A, ES, ESO], f: A => B
  ) extends EffM[M, B, ES, ESO]

  def pure[M[_], ES <: HList, A](a: A): EffM[M, A, ES, ES] = Value(a)

  // def callO[M[_], E <: Effect, ES <: HList, ESO0 <: HList](_eff: E)(
  //   implicit effElem: EffElemO[E, _eff.ResI, _eff.ResO, ES, ESO0]
  // ): EffM[M, _eff.T, ES, ESO0] = new CallP[M, _eff.T, E, ES, ESO0](_eff) {
  //   val prf = new EffElem[E, _eff.ResI, _eff.ResO, ES] {
  //     type ESO = ESO0
  //     def sel(es: ES): MkEff[E, eff.ResI] = effElem.sel(es).asInstanceOf[MkEff[E, eff.ResI]]
  //     def rep(es: ES, r: MkEff[E, eff.ResO]): ESO0 = ??? //effElem.rep(es, r.asInstanceOf[MkEff[E, _eff.ResO]])
  //   }
  //   // effElem.asInstanceOf[EffElemO[E, eff.ResI, eff.ResO, ES, ESO]] // WHY IS IT NEEDED SCALAC????
  // }

  def call[M[_], E <: Effect, ES <: HList](_eff: E)(
    implicit effElem: EffElem[E, _eff.ResI, _eff.ResO, ES]
  ): EffM[M, _eff.T, ES, effElem.ESO] = {
    new CallP[M, _eff.T, E, ES, effElem.ESO](_eff) {
      val prf = effElem.asInstanceOf[EffElem.Aux[E, eff.ResI, eff.ResO, ES, effElem.ESO]]
      // new EffElem[E, eff.ResI, eff.ResO, ES] {
      //   def sel(es: ES): MkEff[E, eff.ResI] = effElem.sel(es).asInstanceOf[MkEff[E, eff.ResI]]
      //   def rep(es: ES, r: MkEff[E, eff.ResO]): effElem.ESO = effElem.rep(es, r.asInstanceOf[MkEff[E, _eff.ResO]])
      // }
    }
  }

  def callM[E <: Effect, ES <: HList](_eff: E)(
    implicit
      ctx: Ctx
    , effElem: EffElem[E, _eff.ResI, _eff.ResO, ES]
  ): EffM[ctx.M, _eff.T, ES, effElem.ESO] = {
    new CallP[ctx.M, _eff.T, E, ES, effElem.ESO](_eff) {
      val prf = effElem.asInstanceOf[EffElem.Aux[E, eff.ResI, eff.ResO, ES, effElem.ESO]]
      // new EffElem[E, eff.ResI, eff.ResO, ES] {
      //   def sel(es: ES): MkEff[E, eff.ResI] = effElem.sel(es).asInstanceOf[MkEff[E, eff.ResI]]
      //   def rep(es: ES, r: MkEff[E, eff.ResO]): effElem.ESO = effElem.rep(es, r.asInstanceOf[MkEff[E, _eff.ResO]])
      // }
    }
  }


  def lift[M[_], A, E <: Effect, ES <: HList, ESO <: HList, Super <: HList, SuperO <: HList](eff: EffM[M, A, ES, ESO])(
    implicit dropE: DropE[ES, Super], rebuildEO: RebuildEO[Super, ESO, SuperO]
  ): EffM[M, A, Super, SuperO] =
    LiftP[M, A, ES, ESO, Super, SuperO](eff, dropE, rebuildEO)

  // def liftI[M[_], A, E <: Effect, ES <: HList, ESO <: HList, Super <: HList, SuperO <: HList](eff: EffM[M, A, ES, ESO])(
  //   implicit dropE: DropE[ES, Super], rebuildE: RebuildE[SuperO, ES, Super]
  // ): EffM[M, A, Super, ESO] =
  //   LiftP[M, A, Super, ES, ESO](eff, dropE, rebuildE)

}


trait EffElemO[E <: Effect, Res, ResO, ES <: HList, ESO <: HList] {
  def sel(es: ES): MkEff[E, Res]
  def rep(es: ES, r: MkEff[E, ResO]): ESO
}

object EffElemO {

  implicit def mkEffElem[E <: Effect, Res, ResO, ES <: HList, ESO <: HList]
    (implicit
      sel0: Lazy[Selector[ES, MkEff[E, Res]]]
    , upd0: Lazy[Rep.Aux[ES, MkEff[E, Res], MkEff[E, ResO], ESO]]
    ) =
    new EffElemO[E, Res, ResO, ES, ESO] {
      //(sel, upd)
      def sel(es: ES): MkEff[E, Res] = sel0.value(es)
      def rep(es: ES, r: MkEff[E, ResO]): ESO = upd0.value(es, r)
    }

}

trait EffElem[E <: Effect, Res, ResO, ES <: HList] {
  type ESO <: HList
  def sel(es: ES): MkEff[E, Res]
  def rep(es: ES, r: MkEff[E, ResO]): ESO
}

object EffElem {
  
  type Aux[E <: Effect, Res, ResO, ES <: HList, ESO0 <: HList] = EffElem[E, Res, ResO, ES] { type ESO = ESO0 }

  implicit def mkEffElem[E <: Effect, Res, ResO, ES <: HList, ESO0 <: HList]
    (implicit
      sel0: Lazy[Selector[ES, MkEff[E, Res]]]
    , upd0: Lazy[Rep.Aux[ES, MkEff[E, Res], MkEff[E, ResO], ESO0]]
    ): EffElem.Aux[E, Res, ResO, ES, ESO0] =
    new EffElem[E, Res, ResO, ES] {
      type ESO = ESO0
      def sel(es: ES): MkEff[E, Res] = sel0.value(es)
      def rep(es: ES, r: MkEff[E, ResO]): ESO0 = upd0.value(es, r)
    }

}

trait Rep[L <: HList, U, V] {
  type Out <: HList
  def apply(l: L, v: V): Out
}

object Rep {
  type Aux[L <: HList, U, V, Out0 <: HList] = Rep[L, U, V] { type Out = Out0 }
  implicit def rep[L <: HList, U, V, Out0 <: HList](implicit rep: Lazy[Replacer.Aux[L, U, V, (U, Out0)]]): Aux[L, U, V, Out0] =
    new Rep[L, U, V] {
      type Out = Out0
      def apply(l: L, v: V): Out = rep.value(l, v)._2
    }
}


trait FlatMappable[ESI <: HList, ESO <: HList, ESI2 <: HList, ESO2 <: HList] {
  type OutESI <: HList
  type OutESO <: HList

  def downI(esi: OutESI): ESI
  def out2in(esi: OutESI, eso: ESO): ESI2
  def buildOut(eso: ESO, eso2: ESO2): OutESO
}


object FlatMappable extends FlatMappable2 {

  type Aux[ESI <: HList, ESO <: HList, ESI2 <: HList, ESO2 <: HList, OutESI0 <: HList, OutESO0 <: HList] =
    FlatMappable[ESI, ESO, ESI2, ESO2] { type OutESI = OutESI0 ; type OutESO = OutESO0 }

  // ESO == ESI2
  implicit def iso[ESI <: HList, ESO <: HList, ESI2 <: HList, ESO2 <: HList](
    implicit  isoI: IsoList[ESO, ESI2],
              // we need to transform internal output into external output
              isoO: IsoList[ESO2, ESO]
  ): FlatMappable.Aux[ESI, ESO, ESI2, ESO2, ESI, ESO] = new FlatMappable[ESI, ESO, ESI2, ESO2] {
    type OutESI = ESI
    type OutESO = ESO

    def downI(esi: ESI): ESI = esi
    def out2in(esi: ESI, eso: ESO): ESI2 = isoI(eso)
    def buildOut(eso: ESO, eso2: ESO2): ESO = isoO(eso2)
  }

  // ESO >> ESI2
  implicit def ESOBiggerThanESI2[ESI <: HList, ESO <: HList, ESI2 <: HList, ESO2 <: HList, ESO3 <: HList](
    implicit  drop: DropE[ESI2, ESO],
              rebuild: RebuildEO[ESO, ESO2, ESO3]
  ): FlatMappable.Aux[ESI, ESO, ESI2, ESO2, ESI, ESO3] = new FlatMappable[ESI, ESO, ESI2, ESO2] {
    type OutESI = ESI
    type OutESO = ESO3

    def downI(esi: ESI): ESI = esi
    def out2in(esi: ESI, eso: ESO): ESI2 = drop.drop(eso)
    def buildOut(eso: ESO, eso2: ESO2): ESO3 = rebuild.rebuild(eso, eso2)
  }


}

trait FlatMappable2 extends FlatMappable3 {

  // ESI << ESI2, ESO << ESI2
  implicit def ESOSmallerThanESI2[ESI <: HList, ESO <: HList, ESI2 <: HList, ESO2 <: HList](
    implicit 
      dropE: DropE[ESI, ESI2]
    , rebuildE: RebuildEO[ESI2, ESO, ESI2]
    , rebuildE2: RebuildEO[ESO, ESO2, ESO2]
  ): FlatMappable.Aux[ESI, ESO, ESI2, ESO2, ESI2, ESO2] = new FlatMappable[ESI, ESO, ESI2, ESO2] {
    type OutESI = ESI2
    type OutESO = ESO2

    def downI(esi: ESI2): ESI = dropE.drop(esi)
    def out2in(esi: ESI2, eso: ESO): ESI2 = rebuildE.rebuild(esi, eso)
    def buildOut(eso: ESO, eso2: ESO2): ESO2 = rebuildE2.rebuild(eso, eso2)
  }

  // ESO == ESI2 with different output type
  // implicit def three[E <: Effect, H, H2, ESI <: HList, ESO <: HList, ESI2 <: HList, ESO2 <: HList, ESO1 <: HList](
  //   implicit
  //     selESO: Selector[ESO, MkEff[E, H]],
  //     remove: Remove.Aux[ESO, MkEff[E, H], (MkEff[E, H], ESO1)],
  //     iso: IsoList[ESO1, ESI2]
  // ): FlatMappable.Aux[ESI, ESO, MkEff[E, H2] :: ESI2, ESO2, ESI, ESO2] = new FlatMappable[ESI, ESO, MkEff[E, H2] :: ESI2, ESO2] {
  //   type OutESI = ESI
  //   type OutESO = ESO2

  //   def downI(esi: ESI): ESI = esi
  //   def upO(esi: ESI, eso: ESO): MkEff[E, H2] :: ESI2 = iso(sel(eso)._2)
  //   def upO2(eso: ESO, eso2: ESO2): ESO2 = eso2
  // }

}

trait FlatMappable3 {

  // ESI ++ ESI2 && ESO ++ ESO2
  implicit def ESIESOPrepend[ESI <: HList, ESO <: HList, ESI2 <: HList, ESO2 <: HList, ESI3 <: HList, ESO3 <: HList](
    implicit prepI: Prepend.Aux[ESI, ESI2, ESI3],
             prepO: Prepend.Aux[ESO, ESO2, ESO3],
             dropI: DropE[ESI, ESI3],
             dropI2: DropE[ESI2, ESI3],
             rebuildEO2: RebuildEO[ESO, ESO2, ESO3]
  ): FlatMappable.Aux[ESI, ESO, ESI2, ESO2, ESI3, ESO3] = new FlatMappable[ESI, ESO, ESI2, ESO2] {
    type OutESI = ESI3
    type OutESO = ESO3

    def downI(esi: ESI3): ESI = dropI.drop(esi)
    def out2in(esi: ESI3, eso: ESO): ESI2 = dropI2.drop(esi)
    def buildOut(eso: ESO, eso2: ESO2): ESO3 = rebuildEO2.rebuild(eso, eso2)
  }
}


trait IsoList[HL1 <: HList, HL2 <: HList] {
  def apply(hl1: HL1): HL2
}

object IsoList {
  implicit def hnil = new IsoList[HNil, HNil] {
    def apply(hl1: HNil): HNil = HNil
  }

  implicit def head[H, HL1 <: HList, HL2 <: HList](implicit iso: IsoList[HL1, HL2]) =
    new IsoList[H :: HL1, H :: HL2] {
      def apply(hl1: H :: HL1): H :: HL2 = hl1.head :: iso(hl1.tail)
    }

  implicit def tail[H, HL1 <: HList, HL2 <: HList, HL3 <: HList](
    implicit  sel: Selector[HL2, H],
              remove: Remove.Aux[HL2, H, (H, HL3)],
              iso: IsoList[HL1, HL3]
  ) = new IsoList[H :: HL1, HL2] {
    def apply(hl1: H :: HL1): HL2 = remove.reinsert(hl1.head -> iso(hl1.tail))
  }
}


/**
 * Type class supporting removal of an element from this `HList`. Available only if this `HList` contains an
 * element of type `E`.
 *
 * @author Stacy Curl
 */
// TEMPORARY UNTIL NEXT SHAPELESS VERSION
@implicitNotFound("Implicit not found: shapeless.Ops.Remove[${L}, ${E}]. You requested to remove an element of type ${E}, but there is no unique candidate in the HList ${L}.")
trait Remove[L <: HList, E] extends DepFn1[L] with Serializable {
  def reinsert(out: Out): L
}

trait LowPriorityRemove {
  type Aux[L <: HList, E, Out0] = Remove[L, E] { type Out = Out0 }

  implicit def recurse[H, T <: HList, E, OutT <: HList](implicit r : Aux[T, E, (E, OutT)]): Aux[H :: T, E, (E, H :: OutT)] =
    new Remove[H :: T, E] {
      type Out = (E, H :: OutT)
      def apply(l : H :: T): Out = {
        val (e, tail) = r(l.tail)
        (e, l.head :: tail)
      }

      def reinsert(out: (E, H :: OutT)): H :: T = out._2.head :: r.reinsert((out._1, out._2.tail))
    }
}

object Remove extends LowPriorityRemove {
  def apply[L <: HList, E](implicit remove: Remove[L, E]): Aux[L, E, remove.Out] = remove

  implicit def remove[H, T <: HList]: Aux[H :: T, H, (H, T)] =
    new Remove[H :: T, H] {
      type Out = (H, T)
      def apply(l : H :: T): Out = (l.head, l.tail)

      def reinsert(out: (H, T)): H :: T = out._1 :: out._2
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



trait RebuildEO[ESI <: HList, ESO <: HList, ESO2 <: HList] {
  def rebuild(esi: ESI, eso: ESO): ESO2
}

object RebuildEO extends RebuildEO2 {

  implicit object nil extends RebuildEO[HNil, HNil, HNil] {
    def rebuild(esi: HNil, eso: HNil): HNil = HNil
  }

  implicit def esoHNil[H, ESI <: HList, ESO <: HList](
    implicit rb: RebuildEO[ESI, HNil, ESO]
  ) = new RebuildEO[H :: ESI, HNil, H :: ESO] {
    def rebuild(esi: H :: ESI, eso: HNil): H :: ESO = esi.head :: rb.rebuild(esi.tail, eso)
  }

  implicit def esiHNil[H, ESO <: HList] = new RebuildEO[HNil, ESO, ESO] {
    def rebuild(esi: HNil, eso: ESO): ESO = eso
  }


  implicit def sameHead[H, ESI <: HList, ESO <: HList, ESO2 <: HList](
    implicit rb: RebuildEO[ESI, ESO, ESO2]
  ) = new RebuildEO[H :: ESI, H :: ESO, H :: ESO2] {
    def rebuild(esi: H :: ESI, eso: H :: ESO): H :: ESO2 = eso.head :: rb.rebuild(esi.tail, eso.tail)
  }

  implicit def esoHead[H, H2, ESI <: HList, ESO <: HList, ESO2 <: HList](
    implicit rb: RebuildEO[ESI, ESO, ESO2]
  ) = new RebuildEO[H :: ESI, H2 :: ESO, H2 :: ESO2] {
    def rebuild(esi: H :: ESI, eso: H2 :: ESO): H2 :: ESO2 = eso.head :: rb.rebuild(esi.tail, eso.tail)
  }


}

trait RebuildEO2 {

  implicit def esiHead[H, H2, ESI <: HList, ESO <: HList, ESO2 <: HList](
    implicit rb: RebuildEO[ESI, H2 :: ESO, ESO2]
  ) = new RebuildEO[H :: ESI, H2 :: ESO, H :: ESO2] {
    def rebuild(esi: H :: ESI, eso: H2 :: ESO): H :: ESO2 = esi.head :: rb.rebuild(esi.tail, eso)
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


trait Ctx {
  type M[_]

  // def app: Applicative[M]
}

object Ctx {
  type Aux[M0[_]] = Ctx { type M[a] = M0[a] }

  // implicit def toApp(ctx: Ctx): Applicative[ctx.M] = ctx.app
}

trait Effectful[M0[_], ESI0 <: HList, ESO0 <: HList] {

  implicit val ctx = new Ctx { type M[a] = M0[a] }

  type ESI = ESI0
  type ESO = ESO0

  type Eff[A] = EffM[M0, A, ESI0, ESO0]

  // implicit val hasESI = new HasESI { type ESI = ESI0 }
  // implicit val hasESO = new HasESO { type ESO = ESI0 }

  // def apply[A](ctx: Ctx): EffM[ctx.M, A, ESI, ESO]

  // def run[M0[_]]() = apply(new Ctx { type M[a] = M0[a] })
}

object Effectful {
  def apply[M[_], ESI <: HList, ESO <: HList] = new Effectful[M, ESI, ESO] { }
}

