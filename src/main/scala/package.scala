
import shapeless._
// import syntax.singleton._
// import record._
// import ops.hlist._
// import syntax.SingletonOps

import scala.language.experimental.macros

package object effects {

  object tag {
    def apply[U] = new Tagger[U]
  }

  type Tagged[U] = shapeless.tag.Tagged[U]
  type @@[+T, U] = shapeless.tag.@@[T, U]

  class Tagger[U] {
    def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U]
  }


  // makes it easier but ugly implicit conversion too :(
  implicit def toTag[T, U](t: T): T @@ U = t.asInstanceOf[T @@ U]
}