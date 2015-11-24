package effects

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import org.scalatest.concurrent.{AsyncAssertions, PatienceConfiguration, ScalaFutures}
import org.scalatest.{Assertions, BeforeAndAfterAll, FeatureSpec, FlatSpec, Matchers, Suite}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import cats.std.future._

import shapeless._


class EffSpec extends FlatSpec with Matchers with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))

  trait Foo
  trait Bar


  "State" should "StateEnv" in {
    val env = Env[Future, MkEff[State, Int @@ Foo] :: MkEff[State, Int @@ Bar] :: HNil]
    import env._

    val stateEnv = StateEnv(env)

    val eff = for {
      _ <- stateEnv.put[Int @@ Foo](5)
      _ <- stateEnv.put[Int @@ Bar](10)
      _ <- stateEnv.update[Int @@ Foo](i => i + 7)
      i <- stateEnv.get[Int @@ Foo]
      _ <- stateEnv.update[Int @@ Bar](j => i + j + 8)
      _ <- stateEnv.putM[Int @@ Foo, String, MkEff[State, String] :: MkEff[State, Int @@ Bar] :: HNil]("toto")      
      j <- stateEnv.getM[Int @@ Bar, MkEff[State, String] :: MkEff[State, Int @@ Bar] :: HNil]
    } yield (j)

    val r = eff.run(MkEff[State, Int @@ Foo](0) :: MkEff[State, Int @@ Bar](5) :: HNil).futureValue

    println("Res:"+r)
    r should equal (30)
  }

  it should "StateEnv0" in {

    type ES = MkEff[State, Int @@ Foo] :: MkEff[State, Int @@ Bar] :: HNil
    type ES2 = MkEff[State, String] :: MkEff[State, Int @@ Bar] :: HNil

    val stateEnv = StateEnv0[Future]()

    val eff = for {
      _ <- stateEnv.put[Int @@ Foo, ES](5)
      _ <- stateEnv.put[Int @@ Bar, ES](10)
      _ <- stateEnv.update[Int @@ Foo, ES](i => i + 7)
      i <- stateEnv.get[Int @@ Foo, ES]
      _ <- stateEnv.update[Int @@ Bar, ES](j => i + j + 8)
      j <- stateEnv.get[Int @@ Bar, ES]
      _ <- stateEnv.putM[Int @@ Foo, String, ES, ES2](s"toto_$j")      
      k <- stateEnv.get[String, ES2]
    } yield (k)

    val r = eff.run(MkEff[State, Int @@ Foo](0) :: MkEff[State, Int @@ Bar](5) :: HNil).futureValue

    println("Res:"+r)
    r should equal ("toto_30")
  }

}