package com.thoughtworks.dsl
package keywords
import Dsl.AsKeyword
import Dsl.Typed




opaque type ForYield[Keyword, AsKeyword.FromKeyword, OutputValue] = Dsl.Typed[Keyword, AsKeyword.FromKeyword]
object ForYield {
  @inline def apply[Keyword, AsKeyword.FromKeyword, OutputValue]: Dsl.Typed[Keyword, AsKeyword.FromKeyword] =:= ForYield[Keyword, AsKeyword.FromKeyword, OutputValue] = summon
}
