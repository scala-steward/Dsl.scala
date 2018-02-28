package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.instructions.Catch

import scala.util.control.NonFatal

/** The state for DSL in exception-handling domain.
  *
  * @author 杨博 (Yang Bo)
  */
trait ExceptionHandling[OtherDomain] extends ((Throwable => OtherDomain) => OtherDomain)

object ExceptionHandling {

  implicit def catchDsl[OtherDomain]
    : Dsl[Catch[ExceptionHandling[OtherDomain]], ExceptionHandling[OtherDomain], Unit] = {
    new Dsl[Catch[ExceptionHandling[OtherDomain]], ExceptionHandling[OtherDomain], Unit] {
      def interpret(instruction: Catch[ExceptionHandling[OtherDomain]],
                    block: Unit => ExceptionHandling[OtherDomain]): ExceptionHandling[OtherDomain] = {
        val Catch(failureHandler) = instruction
        new ExceptionHandling[OtherDomain] {
          def apply(rethrowHandler: Throwable => OtherDomain): OtherDomain = {
            def composedFailureHandler(e: Throwable): OtherDomain = {
              locally {
                try {
                  failureHandler(e)
                } catch {
                  case NonFatal(rethrown) =>
                    return rethrowHandler(rethrown)
                }
              }(rethrowHandler)
            }
            locally {
              try {
                block(())
              } catch {
                case NonFatal(e) =>
                  return composedFailureHandler(e)
              }
            }.apply(composedFailureHandler)
          }

        }
      }

    }
  }

  def success[Domain](r: Domain): ExceptionHandling[Domain] = new ExceptionHandling[Domain] {
    def apply(handler: Throwable => Domain): Domain = r
  }

  def failure[Domain](e: Throwable): ExceptionHandling[Domain] = new ExceptionHandling[Domain] {
    def apply(handler: Throwable => Domain): Domain = handler(e)
  }

  implicit def exceptionHandlingDsl[Instruction, Domain, A](
      implicit restDsl: Dsl[Instruction, Domain, A]): Dsl[Instruction, ExceptionHandling[Domain], A] =
    new Dsl[Instruction, ExceptionHandling[Domain], A] {
      def interpret(instruction: Instruction,
                    successHandler: A => ExceptionHandling[Domain]): ExceptionHandling[Domain] =
        new ExceptionHandling[Domain] {
          def apply(failureHandler: Throwable => Domain): Domain = {
            def restHandler(a: A): Domain =
              (try {
                successHandler(a)
              } catch {
                case NonFatal(e) =>
                  return failureHandler(e)
              }).apply(failureHandler)

            restDsl.interpret(instruction, restHandler)
          }
        }

    }

}
