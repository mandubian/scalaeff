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
import shapeless._

import scala.language.experimental.macros


package object effects extends Effectives {

  object tag {
    def apply[U] = new Tagger[U]
  }

  type Tagged[U] = shapeless.tag.Tagged[U]
  type @@[+T, U] = shapeless.tag.@@[T, U]

  class Tagger[U] {
    def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U]
  }

  implicit def toTag[T, U](t: T): T @@ U = t.asInstanceOf[T @@ U]

  type <>[E <: Effect, T] = MkEff[E, T]

  implicit def liftEff[M[_], A, ESI <: HList, ESO <: HList, Super <: HList, SuperO <: HList](
    eff: EffM[M, A, ESI, ESO]
  )(
    implicit dropE: DropE[ESI, Super], rebuildEO: RebuildEO[Super, ESO, SuperO]
  ): EffM[M, A, Super, SuperO] = eff.lift2[Super, SuperO]

}