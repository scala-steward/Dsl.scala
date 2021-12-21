package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.Dsl
import Dsl.IsKeyword
import scala.util.NotGiven

final case class FlatMap[+Upstream, +Mapped](
    upstream: Upstream,
    flatMapper: _ => Mapped
) extends Dsl.Keyword.Trait

object FlatMap {
  given [Upstream, UpstreamValue, Mapped, MappedValue](using
      IsKeyword[Mapped, MappedValue]
  ): IsKeyword[FlatMap[Upstream, Mapped], MappedValue] with {}

  given [
      Upstream,
      UpstreamValue,
      Mapped,
      MappedValue,
      Domain
  ](using
      upstreamDsl: Dsl[Upstream, Domain, UpstreamValue],
      nestedDsl: Dsl[Mapped, Domain, MappedValue]
  ): Dsl[FlatMap[Upstream, Mapped], Domain, MappedValue] with {
    def cpsApply(
        keyword: FlatMap[Upstream, Mapped],
        handler: MappedValue => Domain
    ): Domain = {
      val FlatMap(upstream, flatMapper) = keyword
      upstreamDsl.cpsApply(
        upstream,
        { upstreamValue =>
          // The typer might erase the type of of parameter of the function
          // when the parameter is a reference to a local value, therefore,
          // we are unable to call `flatMapper` without a cast.
          nestedDsl.cpsApply(flatMapper.asInstanceOf[UpstreamValue => Mapped](upstreamValue), handler)
        }
      )
    }
  }
}
