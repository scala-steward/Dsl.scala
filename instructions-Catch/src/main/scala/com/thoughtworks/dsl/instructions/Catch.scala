package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{Continuation, Instruction, reset}

import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
final case class Catch[Domain](onFailure: Throwable => Domain @reset)
    extends AnyVal
    with Instruction[Catch[Domain], Unit]

object Catch {

  implicit def catchContinuationDsl[Domain, Value](implicit restCatchDsl: Dsl[Catch[Domain], Domain, Unit])
    : Dsl[Catch[Continuation[Domain, Value]], Continuation[Domain, Value], Unit] =
    new Dsl[Catch[Continuation[Domain, Value]], Continuation[Domain, Value], Unit] {

      def interpret(
          instruction: Catch[Continuation[Domain, Value]],
          block: Unit => Continuation[Domain, Value]): Continuation[Domain, Value] = { (continue: Value => Domain) =>
        restCatchDsl.interpret(
          Catch[Domain] { e =>
            instruction.onFailure(e)(continue)
          }, { _: Unit =>
            block(())(continue)
          }
        )
      }
    }

}
