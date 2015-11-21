package eff

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import org.scalatest.concurrent.{AsyncAssertions, PatienceConfiguration, ScalaFutures}
import org.scalatest.{Assertions, BeforeAndAfterAll, FeatureSpec, FlatSpec, Matchers, Suite}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import shapeless._


class EffSpec extends FlatSpec with Matchers with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))

  "Eff" should "have handlers" in {

    implicitly[Handler[Get[Int], Future]]
    implicitly[Handler[Put[Int, String], Future]]

    implicitly[CopHandlers[Future, Get[Int], Put[Int, String] :+: CNil]]
    // val g = Generic[State[_, _, _]]

    // val i: Int :: HNil = g.to(Put[Int, Int](5))
    // implicitly[Handler[State[_, _, _], Future]]

    1 should equal(1)
  }

}