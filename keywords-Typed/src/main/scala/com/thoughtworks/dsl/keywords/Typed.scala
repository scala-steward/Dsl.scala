package com.thoughtworks.dsl
package keywords

/** A type annotated keyword */
opaque type Typed[+Keyword, +Value] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[Keyword]

object Typed {
  given [Keyword, Value]
      : Dsl.IsKeyword[Typed[Keyword, Value], Value] with {}
  given [Keyword, Domain, Value](using
      dsl: Dsl.PolyCont[Keyword, Domain, Value]
  ): Dsl.PolyCont[Typed[Keyword, Value], Domain, Value] =
    dsl

  @inline def apply[Keyword, Value]: Keyword =:= Typed[Keyword, Value] = Dsl.Keyword.Opaque.Of.apply

}
