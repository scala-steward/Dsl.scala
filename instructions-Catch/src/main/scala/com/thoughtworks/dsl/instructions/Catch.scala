package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

import scala.util.control.Exception.Catcher

final case class Catch[Domain](failureHandler: Throwable => Domain) extends AnyVal with Instruction[Catch[Domain], Unit]

object Catch {

//  implicit def catchContinuationDsl[Domain, Value](implicit restCatchDsl: Dsl[Catch[Domain], Domain, Unit])
//    : Dsl[Catch[(Value => Domain) => Domain], (Value => Domain) => Domain, Unit] =
//    new Dsl[Catch[(Value => Domain) => Domain], (Value => Domain) => Domain, Unit] {
//      def interpret(instruction: Catch[(Value => Domain) => Domain],
//                    handler: Unit => (Value => Domain) => Domain): (Value => Domain) => Domain = _ {
//        try {
//          !Shift(handler(())) // TODO: catch exception in block(identity) ?
//        } catch {
//          case e: Throwable =>
//            !Shift(instruction.failureHandler(e))
//        }
//      }
//    }
}
