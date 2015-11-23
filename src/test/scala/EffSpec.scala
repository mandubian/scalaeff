package effects

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import org.scalatest.concurrent.{AsyncAssertions, PatienceConfiguration, ScalaFutures}
import org.scalatest.{Assertions, BeforeAndAfterAll, FeatureSpec, FlatSpec, Matchers, Suite}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import cats.std.future._

import shapeless._
// import syntax.singleton._
// import syntax.SingletonOps
// import tag._
// import record._


class EffSpec extends FlatSpec with Matchers with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))

    trait Foo
    trait Bar

    val env = Env[Future, MkEff[State, Int @@ Foo] :: HNil]
    import env._

    val stateEnv = StateEnv(env)

    val eff = for {
      _ <- stateEnv.put[Int @@ Foo](tag[Foo](5))
      // _ <- stateEnv.update((i:Int) => i + 7)
      i <- stateEnv.get[Int @@ Foo]
    } yield (i)

    val l: Int @@ Foo = tag[Foo](0)

    val r = eff.run(MkEff[State, Int @@ Foo](tag[Foo](0)) :: HNil).futureValue

    println("Res:"+r)
}