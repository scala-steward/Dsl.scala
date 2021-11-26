package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.Typed
import com.thoughtworks.dsl.Dsl.Keyword
import scala.language.higherKinds
import scala.language.implicitConversions

/** A keyword for extracting monadic value from the monadic expression [[fa]].
  *
  * @see [[com.thoughtworks.dsl.domains.cats]] for using this [[Monadic]] keyword with [[cats.Monad]].
  * @see [[com.thoughtworks.dsl.domains.scalaz]] for using this [[Monadic]] keyword with [[scalaz.Monad]].
  * @todo [[Monadic]] should be a [[scala.AnyVal]] after [[https://github.com/scala/bug/issues/10595]] is resolved.
  */
opaque type Monadic[Functor[_], Value] <: Any = Functor[Value]
object Monadic {
  @inline def cast[Functor[_], Value]: Functor[Value] =:= Monadic[Functor, Value] = implicitly
  @inline def apply[Functor[_], Value](f: Functor[Value]): Monadic[Functor, Value] = f

  given [Functor[_], Value]: IsKeyword[Monadic[Functor, Value], Value] with {}

  implicit def implicitMonadic[F[_], A](fa: F[A]): Monadic[F, A] = Monadic(fa)
}
