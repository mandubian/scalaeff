package eff

import shapeless._
import ops.hlist._

import cats.Applicative
import scala.language.higherKinds

/** Effect Algebraic base */
trait Effect {
  type T
  type ResI
  type ResO

  // def handle[A, M[_]](res: ResI)(k: T => ResO => M[A]): M[A]
}

object Effect {
  type Aux[T0, ResI0, ResO0] = Effect { type T = T0; type ResI = ResI0; type ResO = ResO0 }
}

/** EFFECT reification with a resource */
sealed trait MatEffect
case class MkEff[Res, E <: Effect](res: Res) extends MatEffect


sealed trait Env[M[_], ES <: HList]
case class EnvNil[M[_]]() extends Env[M, HNil]
case class EnvCons[M[_], E <: Effect, Res, ES <: HList](head: EffEnv[M, E, Res], tail: Env[M, ES]) extends Env[M, MkEff[Res, E] :: ES]

case class EffEnv[M[_], E <: Effect, Res](
    res: Res
  , handler: Handler[E, M]
) {
  type EFF = MkEff[Res, E]

  def mkEff = MkEff[Res, E](res)
}


sealed trait EffM[M[_], A, ESI <: HList, ESO <: HList] {
  import EffM._

  def run(env: Env[M, ESI])(implicit ap: Applicative[M]): M[A] = eff(env)(a => eso => ap.pure(a))
  
  def runPureEnv(env: Env[M, ESI])(implicit ev: M[A] =:= A, ap: Applicative[M]): M[(Env[M, ESO], A)] =
      eff(env)( a => env => ap.pure((env, a)) )

  def eff[B](env: Env[M, ESI])(ce: A => Env[M, ESO] => M[B]): M[B] = this match {
    case Value(a) => ce(a)(env.asInstanceOf[Env[M, ESO]])
    case EMap(effP, f) => effP.eff(env)(a => envo => ce(f(a))(envo))
    case EBind(effP, f) => effP.eff(env)(a => envo => f(a).eff(envo)(ce))
    case LiftP(effP, dropE, rebuildE) => effP.eff(dropE.drop(env))(a => envo => ce(a)(rebuildE.rebuild(envo, env)))
    case n: New[M, A, r0, e0, es] => n.effP.eff(EnvCons(n.effEnv, env))(a => envo => ce(a)(envo.asInstanceOf[Env[M, ESO]]))
    case c: CallP[M, t, e, es, eso] => c.execEff(env)(a => envo => ce(a)(envo))
  }

  def map[B, ESO2 <: HList](f: A => B) = EMap(this, f)

  def flatMap[B, ESO2 <: HList](f: A => EffM[M, B, ESO, ESO2]) = EBind(this, f)

  def lift[Super <: HList](
    implicit dropE: DropEnv[M, ESI, Super], rebuildE: RebuildEnv[M, ESO, ESI, Super]
  ): EffM[M, A, Super, ESO] =
    LiftP[M, A, Super, ESI, ESO](this, dropE, rebuildE)
}


object EffM {
  case class Value[M[_], ES <: HList, A](a: A) extends EffM[M, A, ES, ES] {
    type T = A
  }

  case class New[M[_], A, R, E <: Effect, ES <: HList](
    effEnv: EffEnv[M, E, R], e: MkEff[R, E], effP: EffM[M, A, MkEff[R, E] :: ES, MkEff[R, E] :: ES]
  ) extends EffM[M, A, ES, ES] {}

  case class LiftP[M[_], A, XS <: HList, YS <: HList, YSO <: HList](
      effP: EffM[M, A, YS, YSO]
    , dropE: DropEnv[M, YS, XS]
    , rebuildE: RebuildEnv[M, YSO, YS, XS]
  ) extends EffM[M, A, XS, YSO] {
    type _Sub = YS
  }

  abstract class CallP[M[_], T, E <: Effect, ES <: HList, ESO <: HList](
    val eff: E
  ) extends EffM[M, T, ES, ESO] {

    val prf: EnvSelector[M, ES, E, eff.ResI]
    val updater: EnvUpdater[M, E, ES, ESO, eff.ResI, eff.ResO]

    def execEff[B](es: Env[M, ES])(ce: T => Env[M, ESO] => M[B]): M[B] = {
      val effEnv: EffEnv[M, E, eff.ResI] = prf(es)
      val res: eff.ResI = effEnv.res
      effEnv.handler.handle(eff)(res) { (a:eff.T) => (resO: eff.ResO) =>
        val eso = updater.apply(es, _ => resO)
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

  def pure[M[_], ES <: HList, A](a: A) = Value(a)

  def call[M[_], E <: Effect, ES <: HList, ESO <: HList](eff: E)(
    implicit sel: EnvSelector[M, ES, E, eff.ResI], upd: EnvUpdater[M, E, ES, ESO, eff.ResI, eff.ResO]
  ) = new CallP[M, eff.T, E, ES, ESO](eff) {
    val prf = sel.asInstanceOf[EnvSelector[M, ES, E, eff.ResI]] // WHY IS IT NEEDED SCALAC????
    val updater = upd.asInstanceOf[EnvUpdater[M, E, ES, ESO, eff.ResI, eff.ResO]]
  }

}

case class EffElem[E <: Effect, Res, ResO, ES <: HList, ESO <: HList](
    sel: Selector[ES, MkEff[Res, E]]
  , rep: Rep.Aux[ES, MkEff[Res, E], MkEff[ResO, E], ESO]
)

object EffElem {
  implicit def mkEffElem[E <: Effect, Res, ResO, ES <: HList, ESO <: HList]
    (implicit
      sel: Selector[ES, MkEff[Res, E]],
      rep: Rep.Aux[ES, MkEff[Res, E], MkEff[ResO, E], ESO]
    ): EffElem[E, Res, ResO, ES, ESO] =
      EffElem[E, Res, ResO, ES, ESO](sel, rep)
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


trait EnvSelector[M[_], ES <: HList, E <: Effect, Res] {
  def apply(env: Env[M, ES]): EffEnv[M, E, Res]
}

object EnvSelector {
  implicit def one[M[_], E <: Effect, Res] = new EnvSelector[M, MkEff[Res, E] :: HNil, E, Res] {
    def apply(env: Env[M, MkEff[Res, E] :: HNil]): EffEnv[M, E, Res] = env match {
      case EnvCons(h, _) => h
    }
  }

  implicit def rec[M[_], E <: Effect, Res, E2 <: Effect, Res2, ES <: HList](implicit next: EnvSelector[M, ES, E, Res]) = new EnvSelector[M, MkEff[Res2, E2] :: ES, E, Res] {
    def apply(env: Env[M, MkEff[Res2, E2] :: ES]): EffEnv[M, E, Res] = env match {
      case EnvCons(_, t) => next(t)
    }
  }
}

trait EnvUpdater[M[_], E <: Effect, ES <: HList, ESO <: HList, Res, ResO] {
  def apply(env: Env[M, ES], f: Res => ResO): Env[M, ESO]
}

object EnvUpdater {

  implicit def one[M[_], E <: Effect, Res, ResO] = new EnvUpdater[M, E, MkEff[Res, E] :: HNil, MkEff[ResO, E] :: HNil, Res, ResO] {
    def apply(env: Env[M, MkEff[Res, E] :: HNil], f: Res => ResO): Env[M, MkEff[ResO, E] :: HNil] = env match {
      case EnvCons(EffEnv(res, h), EnvNil()) => EnvCons(EffEnv[M, E, ResO](f(res), h), EnvNil[M]())
    }
  }

  implicit def rec[M[_], E <: Effect, Res, ResX, ResO, ES <: HList, ESO <: HList](implicit next: EnvUpdater[M, E, ES, ESO, Res, ResO]) =
    new EnvUpdater[M, E, MkEff[ResX, E] :: ES, MkEff[ResX, E] :: ESO, Res, ResO] {
      def apply(env: Env[M, MkEff[ResX, E] :: ES], f: Res => ResO): Env[M, MkEff[ResX, E] :: ESO] = env match {
        case EnvCons(h, t) => EnvCons(h, next(t, f))
      }
    }
}

trait DropEnv[M[_], Small <: HList, Big <: HList] {
  def drop(env: Env[M, Big]): Env[M, Small]
}

object DropEnv {
  implicit def nil[M[_]] = new DropEnv[M, HNil, HNil] {
    def drop(env: Env[M, HNil]): Env[M, HNil] = EnvNil()
  }

  implicit def keep[M[_], H, HL1 <: HList, HL2 <: HList](implicit de: DropEnv[M, HL1, HL2]) = new DropEnv[M, H :: HL1, H :: HL2] {
    def drop(env: Env[M, H :: HL2]): Env[M, H :: HL1] = env match {
      case EnvCons(h, t) => EnvCons(h, de.drop(t))
    }
  }

  implicit def drop[M[_], HL1 <: HList, H, HL2 <: HList](implicit de: DropEnv[M, HL1, HL2]) = new DropEnv[M, HL1, H :: HL2] {
    def drop(env: Env[M, H :: HL2]): Env[M, HL1] = env match {
      case EnvCons(h, t) => de.drop(t)
    }
  }
}

trait RebuildEnv[M[_], Ref <: HList, Small <: HList, Big <: HList] {
  def rebuild(env1: Env[M, Ref], env2: Env[M, Big]): Env[M, Ref]
}

object RebuildEnv extends RebuildEnv2 {

  implicit def nil[M[_]] = new RebuildEnv[M, HNil, HNil, HNil] {
    def rebuild(env1: Env[M, HNil], env2: Env[M, HNil]): Env[M, HNil] = EnvNil()
  }

}

trait RebuildEnv2 extends RebuildEnv3 {

  implicit def rightNil[M[_], H, XS <: HList] = new RebuildEnv[M, H :: XS, HNil, HNil] {
    def rebuild(env1: Env[M, H :: XS], env2: Env[M, HNil]): Env[M, H :: XS] = env1
  }

}

trait RebuildEnv3 extends RebuildEnv4 {

  implicit def leftNil[M[_], XH, XS <: HList] = new RebuildEnv[M, HNil, XH :: XS, XH :: XS] {
    def rebuild(env1: Env[M, HNil], env2: Env[M, XH :: XS]): Env[M, HNil] = env1
  }

  implicit def keepLeft[M[_], XH, XS <: HList, YH, YS <: HList](implicit sub: RebuildEnv[M, YS, XS, XS]) =
    new RebuildEnv[M, YH :: YS , XH :: XS , XH :: XS] {
      def rebuild(env1: Env[M, YH :: YS], env2: Env[M, XH :: XS]): Env[M, YH :: YS] = (env1, env2) match {
        case (EnvCons(h1, t1), EnvCons(h2, t2)) => EnvCons(h1, sub.rebuild(t1, t2))
      }
    }

}

trait RebuildEnv4 {

  implicit def dropRight[M[_], XH, XS <: HList, YS <: HList](implicit sub: RebuildEnv[M, YS, XS, XS]) =
    new RebuildEnv[M, YS, XH :: XS, XH :: XS] {
      def rebuild(env1: Env[M, YS], env2: Env[M, XH :: XS]): Env[M, YS] = env2 match {
        case EnvCons(h, t) => sub.rebuild(env1, t)
      }
    }

}

object Test {
  import scala.concurrent.Future

  // implicitly[SubList[Int :: Double :: String :: HNil, Int :: Double :: String :: HNil]]
  // implicitly[SubList[Int :: String :: HNil, Int :: Double :: String :: HNil]]
  // implicitly[SubList[HNil, HNil]]
  // implicitly[SubList[Int :: Double :: HNil, Int :: Double :: HNil]]
  // implicitly[SubList[Int :: String :: HNil, Int :: Double :: HNil]] // Doesn't compile
  // implicitly[SubList[Double :: Int :: HNil, Int :: Double :: HNil]] // Doesn't compile

  implicitly[DropEnv[Future, Int :: Double :: String :: HNil, Int :: Double :: String :: HNil]]
  implicitly[DropEnv[Future, Int :: String :: HNil, Int :: Double :: String :: HNil]]
  implicitly[DropEnv[Future, HNil, HNil]]
  implicitly[DropEnv[Future, Int :: Double :: HNil, Int :: Double :: HNil]]
  // implicitly[DropEnv[Future, Int :: String :: HNil, Int :: Double :: HNil]] // Doesn't compile
  // implicitly[DropEnv[Future, Double :: Int :: HNil, Int :: Double :: HNil]] // Doesn't compile

  implicitly[RebuildEnv[Future, HNil, HNil,HNil]]
  implicitly[RebuildEnv[Future, Int :: HNil, HNil,HNil]]
  implicitly[RebuildEnv[Future, HNil, Int :: HNil, Int :: HNil]]
  implicitly[RebuildEnv[Future, Int :: Double :: String :: HNil, Int :: Double :: String :: HNil, Int :: Double :: String :: HNil]]
  implicitly[RebuildEnv[Future, Int :: Double :: String :: HNil, Int :: String :: HNil, Int :: String :: HNil]]
  implicitly[RebuildEnv[Future, Int :: String :: HNil, Int :: Double :: HNil, Int :: Double :: HNil]] // should it compile
  implicitly[RebuildEnv[Future, String :: Int :: HNil, Int :: Double :: HNil, Int :: Double :: HNil]] // should it compile
}





  // implicit def handler[M[_]]: Handler[State, M] = new Handler[State, M] {
  //   def handle[A](eff: State)(a: eff.ResI)(k: eff.T => eff.ResO => M[A]): M[A] = //eff.handle(a)(k)
  //     eff match {
  //       case Get() => k(a)(a)
  //     }
  // }

  // type STATE[A] = MkEff[A, State]

  // def get[M[_], A, ES <: HList](implicit el: EffElem[State, A, A, ES, ES]) = EffM.call[M, State, ES, ES](Get[A]())

  // def put[M[_], A, B, ES <: HList](b: B)(implicit el: EffElem[State, A, B, ES, ES]) = EffM.call[M, State, ES, ES](Put[A, B](b))

/*

trait IsEffEnvList[E <: HList]

object IsEffEnvList {
  implicit def hnil = new IsEffEnvList[HNil] {}

  implicit def rec[M[_], E <: Effect, Res, HT <: HList](implicit t: IsEffEnvList[HT]) = new IsEffEnvList[EffEnv[M, E, Res] :: HT] {}
}

trait MatchEffEnvList[E <: HList, F <: HList] {
  def apply(es: E): F
}

object MatchEffEnvList {
  implicit def hnil = new MatchEffEnvList[HNil, HNil] {
    def apply(es: HNil): HNil = HNil
  }

  implicit def rec[M[_], E <: Effect, Res, ET  <: HList, FT <: HList](implicit tailM: MatchEffEnvList[ET, FT]) =
    new MatchEffEnvList[EffEnv[M, E, Res] :: ET, MkEff[Res, E] :: FT] {
      def apply(es: EffEnv[M, E, Res] :: ET): MkEff[Res, E] :: FT = es.head.mkEff :: tailM(es.tail)
    }
}

trait MatchEffEnvListOut[F <: HList, E <: HList] {
  def apply(es: F): E
}

object MatchEffEnvListOut {
  implicit def hnil = new MatchEffEnvListOut[HNil, HNil] {
    def apply(es: HNil): HNil = HNil
  }

  implicit def rec[M[_], E <: Effect, Res, FT <: HList, ET <: HList](
    implicit tailM: MatchEffEnvListOut[FT, ET], handler: Handler[E, M]
  ) = new MatchEffEnvListOut[MkEff[Res, E] :: FT, EffEnv[M, E, Res] :: ET] {
    def apply(es: MkEff[Res, E] :: FT): EffEnv[M, E, Res] :: ET = EffEnv(es.head.res, handler) :: tailM(es.tail)
  }
}
*/
  // def execEff[M[_], B, E <: Effect, ES <: HList, ESO <: HList](es: ES)(eff: E)(ce: eff.T => ESO => M[B])(
  //   implicit  handler: Handler[E, M],
  //             prf: EffElem[E, eff.ResI, eff.ResO, ES, ESO]
  // ): M[B] = {
  //   val ceff: MkEff[eff.ResI, E] = prf.sel(es)
  //   val res: eff.ResI = ceff.res
  //   handler.handle(eff)(res) { (a:eff.T) => (resO: eff.ResO) =>
  //     val eso = prf.rep.apply(es, MkEff(resO))
  //     ce(a)(eso)
  //   }
  // }

/*
trait SubList[Sub <: HList, Super <: HList] {
  def apply(sup: Super): Sub
}

object SubList {

  implicit def hnilSubList = new SubList[HNil, HNil] {
    def apply(sup: HNil): HNil = HNil
  }

  implicit def keepSubList[H, Sub <: HList, Super <: HList](
    implicit sub: SubList[Sub, Super]
  ) = new SubList[H :: Sub, H :: Super] {
    def apply(sup: H :: Super) = sup.head :: sub.apply(sup.tail)
  }

  implicit def dropSubList[H, Sub <: HList, Super <: HList](
    implicit sub: SubList[Sub, Super]
  ) = new SubList[Sub, H :: Super] {
    def apply(sup: H :: Super): Sub = sub.apply(sup.tail)
  }
}

*/

// sealed trait SubList[XS <: HList, YS <: HList]
// case object SubNil extends SubList[HNil, HNil]
// case class Keep[H, XS <: HList, YS <: HList]() extends SubList[H :: XS, H :: YS]
// case class Drop[H, XS <: HList, YS <: HList]() extends SubList[XS, H :: YS]

// object SubList {
//   implicit val hnilSubList: SubList[HNil, HNil] = SubNil
//   implicit def keep[H, XS <: HList, YS <: HList] = Keep[H , XS, YS]()
//   implicit def drop[H, XS <: HList, YS <: HList]: SubList[XS, H :: YS] = Drop[H , XS, YS]()
// }

// trait DropEnv2[M[_], XS <: HList, YS <: HList] {
//   def drop(env: Env[M, YS]): Env[M, XS]
// }

// object DropEnv2 {
//   implicit def dropEnv[M[_], XS <: HList, YS <: HList](implicit sub: SubList[XS, YS]) = new DropEnv2[M, XS, YS]{
//     def drop(env: Env[M, YS]): Env[M, XS] = 
//   }
// }

/*
trait UpdateList[Upd <: HList, Sub <: HList] {
  def update(ext: Upd, sub: Sub): Upd
}

object UpdateList {
  implicit def first[H, Sub <: HList, Super <: HList](
    implicit sublist: SubList[Sub, Super], upd: UpdateList[Super, Sub]
  ) = new UpdateList[H :: Super, H :: Sub] {
        def update(ext: H :: Super, sub: H :: Sub): H :: Super =
          ext.head :: upd.update(ext.tail, sub.tail)
      }

  implicit def second[H, Sub <: HList, Super <: HList](
    implicit sublist: SubList[Sub, Super], upd: UpdateList[Super, Sub]
  ) = new UpdateList[Super, H :: Sub] {
        def update(ext: Super, sub: H :: Sub): H :: Super =
          sub.head :: upd.update(ext, sub.tail)
      }

  implicit def hnilUpd =
    new UpdateList[HNil, HNil] {
      def update(ext: HNil, sub: HNil): HNil = HNil
    }

  implicit def hnilRight[H, Sub <: HList, Super <: HList] =
    new UpdateList[H :: Super, HNil] {
      def update(ext: H :: Super, sub: HNil): H :: Super = ext
    }

  implicit def hnilLeft[H, Sub <: HList, Super <: HList] =
    new UpdateList[HNil, H :: Super] {
      def update(ext: HNil, sub: H :: Super): HNil = ext
    }

}
*/

