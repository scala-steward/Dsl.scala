package com.thoughtworks.dsl

import com.thoughtworks.dsl.Dsl.reset
import com.thoughtworks.dsl.domains.ExceptionHandling
import org.scalatest.{Assertion, AsyncFreeSpec, Matchers}
import task.{Task, _}
import com.thoughtworks.dsl.instructions.{Catch, Each, Fork}

import scala.collection.mutable.ArrayBuffer

/**
  * @author 杨博 (Yang Bo)
  */
final class taskSpec extends AsyncFreeSpec with Matchers {

  "taskToFuture" in taskToFuture(Task.reset {
    succeed
  })

  "loop" in taskToFuture(Task.reset {

    val task1: Task[Int] = Task.now(1)

    val ts = Task.join {
      !Fork(0 until 10) + !task1
    }

    !ts should be(1 until 11)

  })

  "try" in taskToFuture(Task.reset {
    class MyException extends Exception
    val task1: Task[Int] = Task.reset {
      throw new MyException
    }

    val task2 = Task.reset {
      try {
        !task1
        "no exception"
      } catch {
        case myException: MyException =>
          "my exception"
      }
    }

    !task2 should be("my exception")
  })

  "empty try" in {
    val logs = ArrayBuffer.empty[String]

    class MyException extends Exception {
      logs += "MyException"
    }
    val task1: Task[String] = Task.reset {
      throw new MyException
    }

    val task2 = Task.reset {
      try {
        "no exception"
      } catch {
        case myException: MyException =>
          "my exception"
      }
      !task1
    }

    task2 { s =>
      logs += s
      throw new AssertionError()
    } { e =>
      e should be(a[MyException])
      logs += "uncaught MyException"
    }
    logs should be(ArrayBuffer("MyException", "uncaught MyException"))
  }

  "underscore" in {
    val logs = ArrayBuffer.empty[String]

    class MyException extends Exception {
      logs += "MyException"
    }

    val task1: Task[String] = { continue =>
      val x = !Catch[ExceptionHandling[Unit]] {
        case _: MyException =>
          logs += "catched"

          continue("catched"): @reset // FIX compile error when @reset is removed
      }

      throw new MyException

      ExceptionHandling.success(())
    }

    task1.apply { s =>
      logs += s
      throw new AssertionError()
    } { e =>
      println(logs)
      e should be(a[MyException])
      logs += "uncaught MyException"
    }

    logs should be(ArrayBuffer("MyException", "uncaught MyException"))

  }
}
