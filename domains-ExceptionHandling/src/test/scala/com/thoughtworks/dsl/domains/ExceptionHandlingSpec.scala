package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl.reset
import com.thoughtworks.dsl.instructions.{Catch, Shift, Yield}
import org.scalatest.{FreeSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

/**
  * @author 杨博 (Yang Bo)
  */
final class ExceptionHandlingSpec extends FreeSpec with Matchers {
  type AsyncFunction[Domain, +A] = (A => Domain) => Domain

  "manually call" - {
    "Catch" in {
      object MyException extends Exception

      var logs = ArrayBuffer.empty[String]
      def generator1: (Int => ExceptionHandling[Stream[String]]) => ExceptionHandling[Stream[String]] = { continue =>
        !Catch[ExceptionHandling[Stream[String]]] {
          case MyException =>
            logs += "begin catch"
            !Yield("catch")
            logs += "end catch"

            continue(43): @reset // FIX compile error when @reset is removed
        }
        logs += "begin last yield"
        !Yield("last yield")
        logs += "end last yield"

        throw MyException

        !Catch[ExceptionHandling[Stream[String]]] {
          case MyException =>
            logs += "begin catch"
            !Yield("catch")
            logs += "end catch"

            ExceptionHandling.success(Stream.empty[String]): @reset // FIX compile error when @reset is removed
        }

        continue(42)
      }
      val result = generator1 { i =>
        i should be(43)
        ExceptionHandling.success(Stream.empty[String])
      } { e =>
        throw e
      }

      result.length

      logs should be(ArrayBuffer("begin last yield", "end last yield", "begin catch", "end catch"))

      result should be(Stream("last yield", "catch"))

      def generator2: ExceptionHandling[Stream[String]] = {
        !Shift(generator1) should be(43)
        (throw MyException): ExceptionHandling[Stream[String]]
      }

      val x = generator2 { e =>
        throw e
      }

      a[MyException.type] should be thrownBy generator2 { e =>
        throw e
      }.length

    }
  }

  "Given a continuation that throws an exception" - {
    object MyException extends Exception

    "Given a generator" - {

      object MyException extends Exception
      def generator: ExceptionHandling[Stream[Int]] = {
        !Yield(1)
        throw {
          !Yield(2)
          MyException
        }
        !Yield(3)
        ExceptionHandling.success(Stream.empty)
      }

      "When catching exception thrown from the generator" - {
        val catching = generator.apply { e: Throwable =>
          e should be(MyException)
          Stream(100)
        }
        "Then it should contain all elements before throwing exception and the element when catching the exception" in {
          catching should be(Seq(1, 2, 100))
        }
      }

    }
  }
  "try/catch" - {
    "yield and catch" in {
      def continuation: AsyncFunction[ExceptionHandling[Stream[Int]], String] = _ {
        val tryResult = try {
          0 / 0
        } catch {
          case e: ArithmeticException =>
            !Yield(3)
            "catch"
        }
        "returns " + tryResult
      }
      continuation { result: String =>
        result should be("returns catch")
        ExceptionHandling.success(Stream.empty)
      }.apply { e =>
        Stream.empty
      } should be(Seq(3))
    }

    "issue 2" in {

      def emptyTry: AsyncFunction[ExceptionHandling[Stream[Int]], Unit] = _ {

        try {} catch {
          case e: ArithmeticException =>
            !Yield(2)
        }
      }

      def continuation: AsyncFunction[ExceptionHandling[Stream[Int]], String] = _ {
        !Yield(1)
        !Shift(emptyTry)
        !Yield(3)
        0 / 0
        "OK"
      }

      continuation { result: String =>
        ExceptionHandling.failure(new AssertionError())
      }.apply { e =>
        e should be(a[ArithmeticException])
        Stream.empty
      } should be(Stream(1, 3))

    }

    "empty try" in {
      def continuation: AsyncFunction[ExceptionHandling[Stream[Int]], String] = _ {
        val tryResult = try {
          0 / 0
          !Yield(-1)
        } finally {}
        "returns " + tryResult
      }
      continuation { result: String =>
        ExceptionHandling.failure(new AssertionError())
      }.apply { e =>
        e should be(a[ArithmeticException])
        Stream.empty
      } should be(Seq())
    }
    "rethrow" in {
      def continuation: AsyncFunction[ExceptionHandling[Stream[Int]], String] = _ {
        val tryResult = try {
          0 / 0
        } catch {
          case e: ArithmeticException =>
            !Yield(42)
            throw e
        }
        "returns " + tryResult
      }
      continuation { result: String =>
        ExceptionHandling.failure(new AssertionError())
      }.apply { e =>
        e should be(a[ArithmeticException])
        Stream.empty
      } should be(Seq(42))
    }

    "complex" in {
      def continuation: AsyncFunction[ExceptionHandling[Stream[Int]], String] = _ {
        !Yield(0)
        val tryResult = try {
          !Yield(1)
          try {} catch {
            case e: ArithmeticException =>
              !Yield(2)
          }
          !Yield(3)

          0 / 0
          !Yield(4)
          "try"
        } catch {
          case e: ArithmeticException =>
            !Yield(5)
            "catch"
        } finally {
          !Yield(6)

          def ignoredFinalResult = "finally"
          ignoredFinalResult
        }
        !Yield(7)
        "returns " + tryResult
      }

      continuation { result: String =>
        result should be("returns catch")
        ExceptionHandling.success(Stream.empty)
      }.apply { e =>
        Stream.empty
      } should be(Seq(0, 1, 3, 5, 6, 7))
    }
  }
}
