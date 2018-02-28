package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.instructions.Catch

import scala.util.Try
import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/** The state for DSL in exception-handling domain.
  *
  * @author 杨博 (Yang Bo)
  */
trait ExceptionHandling[OtherDomain] extends ((Throwable => OtherDomain) => OtherDomain)

object ExceptionHandling {

  implicit def catchDsl[OtherDomain]: Dsl[Catch[ExceptionHandling[OtherDomain]],
                                          ExceptionHandling[OtherDomain],
                                          ExceptionHandling[OtherDomain] => ExceptionHandling[OtherDomain]] = {
    new Dsl[Catch[ExceptionHandling[OtherDomain]],
            ExceptionHandling[OtherDomain],
            ExceptionHandling[OtherDomain] => ExceptionHandling[OtherDomain]] {
      def interpret(
          instruction: Catch[ExceptionHandling[OtherDomain]],
          block: (
              ExceptionHandling[OtherDomain] => ExceptionHandling[OtherDomain]) => ExceptionHandling[OtherDomain])
        : ExceptionHandling[OtherDomain] = {

        new ExceptionHandling[OtherDomain] {
          def apply(restFailureHandler: Throwable => OtherDomain): OtherDomain = {
            def failureHandler(e: Throwable): OtherDomain = {
              locally {
                try {
                  instruction.onFailure(e)
                } catch {
                  case NonFatal(rethrown) =>
                    return restFailureHandler(rethrown)
                }
              }.apply(restFailureHandler)
            }

            locally {
              try {
                block { (blockResult: ExceptionHandling[OtherDomain]) =>
                  ExceptionHandling.success(blockResult(failureHandler))
//                  ExceptionHandling.success(blockResult(failureHandler))
                }
              } catch {
                case NonFatal(e) => return restFailureHandler(e)
              }
            }.apply(restFailureHandler)
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
