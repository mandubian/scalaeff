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
}

object Effect {
  type Aux[T0, ResI0, ResO0] = Effect { type T = T0; type ResI = ResI0; type ResO = ResO0 }
}

trait Handler[E <: Effect, M[_]] {
  type T
  type ResI
  type ResO

  def handle[A](e: E)(res: ResI)(k: T => ResO => M[A]): M[A]
}

object Handler {
  type Aux[E <: Effect, T0, ResI0, ResO0, M[_]] = Handler[E, M] { type T = T0; type ResI = ResI0; type ResO = ResO0 }
}

/** EFFECT reification with a resource */
trait EffectM
case class MkEff[E <: Effect, Res](res: Res) extends EffectM

sealed trait EffM[M[_], A, ESI <: HList, ESO <: HList, HS <: HList] {
  import EffM._

  def run[ESI0 <: HList](es: ESI0)(
    implicit  ap: Applicative[M]
            , iso: IsoList[ESI0, ESI]
            , hs: Handlers[M, HS]
  ): M[A] = eff(hs.handlers)(iso(es))(a => eso => ap.pure(a))
  
  def runPure(es: ESI)(
    implicit  ev: M[A] =:= A, ap: Applicative[M]
            , hs: Handlers[M, HS]
  ): M[(ESO, A)] =
      eff(hs.handlers)(es)( a => env => ap.pure((env, a)) )

  def eff[B](hs: HS)(es: ESI)(ce: A => ESO => M[B]): M[B] = this match {
    case Value(a) => ce(a)(es.asInstanceOf[ESO])
    case EMap(effP, f) => effP.eff(hs)(es)(a => eso => ce(f(a))(eso))

    case EFlatMap(effP, f, prf, merge) =>
          val (hs1, hs2) = merge.resplit(hs)
          effP.eff(hs1)(prf.downI(es)) { a => eso =>
            f(a).eff(hs2)(prf.out2in(es, eso)) { a => eso2 =>
              ce(a)(prf.buildOut(eso, eso2))
            }
          }

    // case EBind(effP, f) => effP.eff(es)(a => eso => f(a).eff(eso)(ce))
    case l@LiftP(effP, dropE, rebuildEO) => effP.eff(hs)(dropE.drop(es))(a => eso => ce(a)(rebuildEO.rebuild(es, eso)))
    case New(e, h, effP) => effP.eff(h :: hs)(e :: es)(a => eso => ce(a)(eso.tail))
    case c: CallP[M, t, e, e1, es, eso, hs] => c.execEff(hs)(es)(a => eso => ce(a)(eso))
  }

  def map[B](f: A => B): EffM[M, B, ESI, ESO, HS] = EMap[M, A, B, ESI, ESO, HS](this, f)

  def flatMap[B, ESI2 <: HList, ESO2 <: HList, ESI3 <: HList, ESO3 <: HList, HS2 <: HList, HS3 <: HList](f: A => EffM[M, B, ESI2, ESO2, HS2])(
    implicit
      flatMappable: FlatMappable.Aux[ESI, ESO, ESI2, ESO2, ESI3, ESO3],
      merge: Merge.Aux[HS, HS2, HS3]
  ): EffM[M, B, ESI3, ESO3, HS3] = EFlatMap(this, f, flatMappable, merge)

  // def lift2[Super <: HList, SuperO <: HList](
  //   implicit dropE: DropE[ESI, Super], rebuildEO: RebuildEO[Super, ESO, SuperO]
  // ): EffM[M, A, Super, SuperO] =
  //   LiftP[M, A, ESI, ESO, Super, SuperO](this, dropE, rebuildEO)

  def lift[Super <: HList](
    implicit dropE: DropE[ESI, Super], rebuildEO: RebuildEO[Super, ESO, Super]
  ): EffM[M, A, Super, Super, HS] =
    LiftP[M, A, ESI, ESO, Super, Super, HS](this, dropE, rebuildEO)

}


object EffM {
  case class Value[M[_], ES <: HList, HS <: HList, A](a: A) extends EffM[M, A, ES, ES, HS] {
    type T = A
  }
 
  case class New[M[_], A, R, E <: Effect, ES <: HList, ESO <: HList, HS <: HList](
    e: MkEff[E, R], handler: Handler[E, M], effP: EffM[M, A, MkEff[E, R] :: ES, MkEff[E, R] :: ESO, Handler[E, M] :: HS]
  ) extends EffM[M, A, ES, ESO, HS] {}

  case class LiftP[M[_], A, ESI <: HList, ESO <: HList, ESI2 <: HList, ESO2 <: HList, HS <: HList](
      effP: EffM[M, A, ESI, ESO, HS]
    , dropE: DropE[ESI, ESI2]
    , rebuildEO: RebuildEO[ESI2, ESO, ESO2]
  ) extends EffM[M, A, ESI2, ESO2, HS] {
    type _Sub = ESI
    type _SubO = ESO
  }

  abstract class CallP[M[_], T, E <: Effect, E1 <: E, ES <: HList, ESO <: HList, HS <: HList](
    val eff: E1
  ) extends EffM[M, T, ES, ESO, HS] {

    def prf: EffElem.Aux[E, eff.ResI, eff.ResO, ES, ESO]
    def hdprf: HandlerElem[M, E1, HS]

    def execEff[B](hs: HS)(es: ES)(ce: T => ESO => M[B]): M[B] = {
      val m: MkEff[E, eff.ResI] = prf.sel(es)
      val handler: Handler.Aux[E1, eff.T, eff.ResI, eff.ResO, M] = hdprf.sel(hs).asInstanceOf[Handler.Aux[E1, eff.T, eff.ResI, eff.ResO, M]]
      val res: eff.ResI = m.res
      handler.handle(eff)(res) { (a:eff.T) => (resO: eff.ResO) =>
        val eso = prf.rep(es, MkEff[E, eff.ResO](resO))
        ce(a.asInstanceOf[T])(eso)
      }
    }

  }

  case class EFlatMap[
    M[_], A, B,
    ESI <: HList, ESO <: HList,
    ESI2 <: HList, ESO2 <: HList,
    ESI3 <: HList, ESO3 <: HList,
    HS <: HList, HS2 <: HList, HS3 <: HList
  ](
    effP: EffM[M, A, ESI, ESO, HS]
  , f: A => EffM[M, B, ESI2, ESO2, HS2]
  , prf: FlatMappable.Aux[ESI, ESO, ESI2, ESO2, ESI3, ESO3]
  , merge: Merge.Aux[HS, HS2, HS3]
  ) extends EffM[M, B, ESI3, ESO3, HS3]

  // case class EBind[M[_], A, B, ES <: HList, ESO <: HList, ESO2 <: HList](
  //   effP: EffM[M, A, ES, ESO], f: A => EffM[M, B, ESO, ESO2]
  // ) extends EffM[M, B, ES, ESO2] {
  //   type _ESO = ESO
  // }

  case class EMap[M[_], A, B, ES <: HList, ESO <: HList, HS <: HList](
    effP: EffM[M, A, ES, ESO, HS], f: A => B
  ) extends EffM[M, B, ES, ESO, HS]

  def pure[M[_], ES <: HList, A](a: A): EffM[M, A, ES, ES, HNil] = Value(a)

  def call[M[_], E <: Effect, E1 <: E, ES <: HList, HS <: HList](_eff: E1)(
    implicit 
      effElem: EffElem[E, _eff.ResI, _eff.ResO, ES],
      handlerElem: HandlerElem[M, E1, HS]
  ): EffM[M, _eff.T, ES, effElem.ESO, HS] = {
    new CallP[M, _eff.T, E, E1, ES, effElem.ESO, HS](_eff) {
      val prf = effElem.asInstanceOf[EffElem.Aux[E, eff.ResI, eff.ResO, ES, effElem.ESO]]

      val hdprf = handlerElem
    }
  }

  // def callM[E <: Effect, E1 <: E, ES <: HList, M[_]](_eff: E1)(
  //   implicit
  //     ctx: Ctx.Aux[M]
  //   , effElem: EffElem[E, _eff.ResI, _eff.ResO, ES]
  // ): EffM[M, _eff.T, ES, effElem.ESO] = {
  //   new CallP[M, _eff.T, E, E1, ES, effElem.ESO](_eff) {
  //     val prf = effElem.asInstanceOf[EffElem.Aux[E, eff.ResI, eff.ResO, ES, effElem.ESO]]
  //     val handler = handler0
  //   }
  // }


  def lift[M[_], A, E <: Effect, ES <: HList, ESO <: HList, Super <: HList, SuperO <: HList, HS <: HList](
    eff: EffM[M, A, ES, ESO, HS]
  )(
    implicit dropE: DropE[ES, Super], rebuildEO: RebuildEO[Super, ESO, SuperO]
  ): EffM[M, A, Super, SuperO, HS] =
    LiftP[M, A, ES, ESO, Super, SuperO, HS](eff, dropE, rebuildEO)

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


trait HandlerElem[M[_], E <: Effect, HS <: HList] {
  def sel(hs: HS): Handler[E, M]
}

object HandlerElem {

  implicit def mkHandlerElem[M[_], E <: Effect, HS <: HList]
    (implicit
      sel0: Lazy[Selector[HS, Handler[E, M]]]
    ): HandlerElem[M, E, HS] =
    new HandlerElem[M, E, HS] {
      def sel(hs: HS): Handler[E, M] = sel0.value(hs)
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
  implicit def ESIAndESOSmallerThanESI2[ESI <: HList, ESO <: HList, ESI2 <: HList, ESO2 <: HList](
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

}

trait FlatMappable3 {

  // MkEff[S, A1] :: MkEff[U, B] :: MkEff[W, D] => MkEff[S, A2] => MkEff[S, A2] :: MkEff[U, B] :: MkEff[V, C]

  // ESO << ESI2 but different type
  implicit def ESOSmallerThanESI2Different[ESI <: HList, ESO <: HList, ESI2 <: HList, ESO2 <: HList, ESII <: HList, ESIII <: HList](
    implicit 
      dropE: DropE[ESO, ESI2]
    , removeAll: RemoveAll.Aux[ESI2, ESO, (ESO, ESII)]
    , merge: Merge.Aux[ESI, ESII, ESIII]
    , dropEI: DropE[ESI, ESIII]
    , rebuildE2: RebuildEO[ESO, ESO2, ESO2]
  ): FlatMappable.Aux[ESI, ESO, ESI2, ESO2, ESIII, ESO2] = new FlatMappable[ESI, ESO, ESI2, ESO2] {
    type OutESI = ESIII
    type OutESO = ESO2

    def downI(esi: ESIII): ESI = dropEI.drop(esi)
    def out2in(esiii: ESIII, eso: ESO): ESI2 = {
      val (esi, esii) = merge.resplit(esiii)
      removeAll.reinsert((eso, esii))
    }
    def buildOut(eso: ESO, eso2: ESO2): ESO2 = rebuildE2.rebuild(eso, eso2)
  }

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

/**
 * Type class supporting removal of a sublist from this `HList`. Available only if this `HList` contains a
 * sublist of type `SL`.
 *
 * The elements of `SL` do not have to be contiguous in this `HList`.
 *
 * @author Stacy Curl
 */
@implicitNotFound("Implicit not found: shapeless.Ops.RemoveAll[${L}, ${SL}]. You requested to remove elements of the types ${SL}, but not all were found in HList ${L}.")
trait RemoveAll[L <: HList, SL <: HList] extends DepFn1[L] with Serializable {
  def reinsert(out: Out): L
}

object RemoveAll {
  def apply[L <: HList, SL <: HList](implicit remove: RemoveAll[L, SL]): Aux[L, SL, remove.Out] = remove

  type Aux[L <: HList, SL <: HList, Out0] = RemoveAll[L, SL] { type Out = Out0 }

  implicit def hlistRemoveAllNil[L <: HList]: Aux[L, HNil, (HNil, L)] =
    new RemoveAll[L, HNil] {
      type Out = (HNil, L)
      def apply(l : L): Out = (HNil, l)

      def reinsert(out: (HNil, L)): L = out._2
    }

  implicit def hlistRemoveAll[L <: HList, E, RemE <: HList, Rem <: HList, SLT <: HList]
    (implicit rt : Remove.Aux[L, E, (E, RemE)], st : Aux[RemE, SLT, (SLT, Rem)]): Aux[L, E :: SLT, (E :: SLT, Rem)] =
      new RemoveAll[L, E :: SLT] {
        type Out = (E :: SLT, Rem)
        def apply(l : L): Out = {
          val (e, rem) = rt(l)
          val (sl, left) = st(rem)
          (e :: sl, left)
        }

        def reinsert(out: (E :: SLT, Rem)): L =
          rt.reinsert((out._1.head, st.reinsert((out._1.tail, out._2))))
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

trait Merge[HL1 <: HList, HL2 <: HList] {
  type Out <: HList
  def merge(hl1: HL1, hl2: HL2): Out
  def resplit(out: Out): (HL1, HL2)
}

object Merge extends Merge2 {
  type Aux[HL1 <: HList, HL2 <: HList, Out0 <: HList] = Merge[HL1, HL2] { type Out = Out0 }

  // implicit val nil: Merge.Aux[HNil, HNil, HNil] = new Merge[HNil, HNil] {
  //   type Out = HNil
  //   def merge(hl1: HNil, hl2: HNil): HNil = HNil
  // }

  implicit def rightNil[HL2 <: HList]: Merge.Aux[HNil, HL2, HL2] = new Merge[HNil, HL2] {
    type Out = HL2
    def merge(hl1: HNil, hl2: HL2): HL2 = hl2

    def resplit(out: HL2): (HNil, HL2) = (HNil, out)
  }

  implicit def leftNil[HL1 <: HList]: Merge.Aux[HL1, HNil, HL1] = new Merge[HL1, HNil] {
    type Out = HL1
    def merge(hl1: HL1, hl2: HNil): HL1 = hl1

    def resplit(out: HL1): (HL1, HNil) = (out, HNil)
  }

  implicit def headInHL2[H1, HL1 <: HList, HL2 <: HList, HLO <: HList, HLO2 <: HList](
    implicit rem: Remove.Aux[HL2, H1, (H1, HLO)],
             sub: Merge.Aux[HL1, HLO, HLO2]
  ): Merge.Aux[H1 :: HL1, HL2, H1 :: HLO2] = new Merge[H1 :: HL1, HL2] {
    type Out = H1 :: HLO2
    def merge(hl1: H1 :: HL1, hl2: HL2): H1 :: HLO2 = {
      val (h1, hlo) = rem(hl2)
      h1 :: sub.merge(hl1.tail, hlo)
    }

    def resplit(out: H1 :: HLO2): (H1 :: HL1, HL2) = {
      val (l1, l2) = sub.resplit(out.tail)
      (out.head :: l1, rem.reinsert(out.head, l2))
    }
  }

}

trait Merge2 {
  implicit def headNotInHL2[H1, HL1 <: HList, HL2 <: HList, HLO <: HList](
    implicit sub: Merge.Aux[HL1, HL2, HLO]
  ): Merge.Aux[H1 :: HL1, HL2, H1 :: HLO] = new Merge[H1 :: HL1, HL2] {
    type Out = H1 :: HLO
    def merge(hl1: H1 :: HL1, hl2: HL2): H1 :: HLO = {
      hl1.head :: sub.merge(hl1.tail, hl2)
    }

    def resplit(out: H1 :: HLO): (H1 :: HL1, HL2) = {
      val (l1, l2) = sub.resplit(out.tail)
      (out.head :: l1, l2)
    }
  }
}

trait Handlers[M[_], HS <: HList] {
  val handlers: HS
}

object Handlers {

  implicit def nil[M[_]]: Handlers[M, HNil] = new Handlers[M, HNil] {
    val handlers = HNil
  }

  implicit def head[M[_], E <: Effect, HS <: HList](
    implicit
        handler: Handler[E, M]
      , next: Handlers[M, HS]
    ): Handlers[M, Handler[E, M] :: HS] =
    new Handlers[M, Handler[E, M] :: HS] {
      val handlers = handler :: next.handlers
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

  // type Eff[A] = EffM[M0, A, ESI0, ESO0]

}

object Effectful {
  def apply[M[_], ESI <: HList, ESO <: HList] = new Effectful[M, ESI, ESO] { }
}

