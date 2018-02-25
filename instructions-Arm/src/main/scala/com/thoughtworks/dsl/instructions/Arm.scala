package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction
import com.thoughworks.dsl.Raii
import resource.Resource

/**
  * @author 杨博 (Yang Bo)
  */
final case class Arm[R](r: R)(implicit val resource: Resource[R]) extends Instruction[Arm[R], R]

object Arm {

  implicit def armDsl[Domain, R]: Dsl[Arm[R], Raii[Domain, R], R] = new Dsl[Arm[R], Raii[Domain, R], R] {
    def interpret(arm: Arm[R], handler: R => Raii[Domain, R]): Raii[Domain, R] = {
      val r = arm.r
      val resource = arm.resource
      new Raii[Domain, R] {
        def using(handler: R => Domain): Domain = {
          try {
            resource.open(r)
            handler(r)
          } finally {
            resource.close(r)
          }
        }
      }
    }
  }
}
