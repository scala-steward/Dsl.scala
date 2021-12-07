package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.cpsApply
import Dsl.Typed

case class While[
    ConditionKeyword,
    BodyKeyword
](
    condition: ConditionKeyword,
    body: BodyKeyword
)
object While {
  given [
      ConditionKeyword,
      BodyKeyword,
      Domain
  ](using
      conditionDsl: Dsl.PolyCont[ConditionKeyword, Domain, Boolean],
      bodyDsl: Dsl.PolyCont[BodyKeyword, Domain, Any]
  ): Dsl.PolyCont[
    While[ConditionKeyword, BodyKeyword],
    Domain,
    Unit
  ] with {
    def cpsApply(
        keyword: While[ConditionKeyword, BodyKeyword],
        handler: Unit => Domain
    ): Domain = {
      keyword.condition.cpsApply {
        case true =>
          keyword.body.cpsApply { _ =>
            cpsApply(keyword, handler)
          }
        case false =>
          handler(())
      }
    }

  }
}
