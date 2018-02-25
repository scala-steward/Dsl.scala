package com.thoughtworks.dsl.instructions

import com.thoughworks.dsl.Raii
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
class ArmSpec extends FreeSpec with Matchers {

  "AutoCloseable" - {
//
//    "using" in {
//      var isOpen = false
//      def autoCloseable: Unit = {
//        isOpen should be(false)
//        !Arm(new AutoCloseable {
//          def close(): Unit = {
//            isOpen should be(true)
//            isOpen = false
//          }
//        })
//        isOpen should be(true)
//      }
//      isOpen should be(false)
//    }

    "continuation" - {

      "using" in {
        var isOpen = false
        def raii: Raii[Unit, AutoCloseable] = {
          isOpen should be(false)
          val a = !Arm(new AutoCloseable {
            isOpen should be(false)
            isOpen = true
            def close(): Unit = {
              isOpen should be(true)
              isOpen = false
            }
          })
          isOpen should be(true)
          Raii.move(a)
        }

        raii.using { resource =>
          isOpen should be(true)
        }
        isOpen should be(false)
      }
    }
  }

}
