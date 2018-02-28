package com.thoughtworks.dsl.instructions

import cats.FlatMap
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction
import cats.MonadError

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
final case class CatsFlatMap[F[_], A](fa: F[A]) extends AnyVal with Instruction[CatsFlatMap[F, A], A]

object CatsFlatMap {

  implicit def catsCatchDsl[F[_], A](implicit monadError: MonadError[F, Throwable]): Dsl[Catch[F[A]], F[A], Unit] =
    new Dsl[Catch[F[A]], F[A], Unit] {
      def interpret(instruction: Catch[F[A]], rest: Unit => F[A]): F[A] = {

        def exceptionHandler(e: Throwable): F[A] = {
          try {
            instruction.failureHandler(e)
          } catch {
            case NonFatal(rethrown) =>
              monadError.raiseError(rethrown)
          }
        }

        monadError.handleErrorWith(try {
          rest(())
        } catch {
          case NonFatal(e) =>
            return exceptionHandler(e)
        })(exceptionHandler)
      }
    }

  implicit def implicitCatsFlatMap[F[_], A](fa: F[A]): CatsFlatMap[F, A] = CatsFlatMap(fa)

  implicit def catsFlatMapDsl[F[_], A, B](implicit flatMap: FlatMap[F]): Dsl[CatsFlatMap[F, A], F[B], A] =
    new Dsl[CatsFlatMap[F, A], F[B], A] {
      def interpret(instruction: CatsFlatMap[F, A], handler: A => F[B]): F[B] = {
        flatMap.flatMap(instruction.fa)(handler)
      }
    }

}
