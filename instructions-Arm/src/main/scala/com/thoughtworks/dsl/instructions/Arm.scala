package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{Continuation, Instruction}
import resource.Resource

import scala.language.implicitConversions
import scala.util.Try

/**
  * @author 杨博 (Yang Bo)
  */
final case class Arm[R](resourceFactory: () => R, resource: Resource[R]) extends Instruction[Arm[R], R]

object Arm {

  implicit def implicitArm[R: Resource](r: => R): Arm[R] = Arm[R](r)

  def apply[R](r: => R)(implicit resource: Resource[R], dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit) = {
    new Arm[R](r _, resource)
  }

  implicit def armDsl[Domain, A, R](implicit scopeDsl: Dsl[Scope[Domain, Try[A]], Domain, Try[A]],
                                    catchDsl: Dsl[Catch[Domain], Domain, Unit]): Dsl[Arm[R], Domain, R] =
    new Dsl[Arm[R], Domain, R] {
      def interpret(instruction: Arm[R], inUse: R => Domain): Domain = {
        val Arm(resourceFactory, resource) = instruction
        val r = resourceFactory()

        Catch[Domain] { e: Throwable =>
          ???
        }.cpsApply[Domain] { _: Unit =>
          inUse(r)
        }

//        val r = resourceFactory()
//        try {
//          inUse(r)
//        } finally {
//          resource.close(r)
//        }
      }
    }

  //  implicit def armDsl[Domain, R, A](
//      implicit scopeDsl: Dsl[Scope[Domain, Try[A]], Domain, Try[A]],
//      catchDsl: Dsl[Catch[Domain], Domain, Unit]): Dsl[Arm[R], Continuation[Domain, A], R] =
//    new Dsl[Arm[R], Continuation[Domain, A], R] {
//      def interpret(arm: Arm[R], inUse: R => Continuation[Domain, A]): ((A => Domain) => Domain) = {
//        val Arm(resourceFactory, resource) = arm
//        _ {
//          val r = resourceFactory()
//          try {
//            resource.open(r)
//            !Shift(inUse(r))
//          } finally {
//            resource.close(r)
//          }
//        }
//      }
//    }

}
