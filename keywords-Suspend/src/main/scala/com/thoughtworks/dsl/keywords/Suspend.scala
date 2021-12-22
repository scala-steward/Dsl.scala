package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.cpsApply

opaque type Suspend[+Keyword] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[() => Keyword]
object Suspend {
  @inline def apply[Keyword]: (() => Keyword) =:= Suspend[Keyword] = Dsl.Keyword.Opaque.Of.apply

  given[Upstream, UpstreamValue](using upstreamIsKeyword: => IsKeyword[Upstream, UpstreamValue]): IsKeyword[Suspend[Upstream], UpstreamValue] with {}

  given [Keyword, Domain, Value](using
      Dsl.Searching[Keyword, Domain, Value]
  ): Dsl.Composed[Suspend[Keyword], Domain, Value] = Dsl.Composed {
    (keyword: Suspend[Keyword], handler: Value => Domain) =>
      keyword().cpsApply(handler)
  }

}
