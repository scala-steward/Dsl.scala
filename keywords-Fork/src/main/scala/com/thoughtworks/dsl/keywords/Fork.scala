package com.thoughtworks.dsl.keywords

import java.io.{PrintStream, PrintWriter}
import java.util.concurrent.atomic.AtomicInteger

import com.thoughtworks.dsl.bangnotation.{`*`, unary_!}
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, IsKeyword}
import com.thoughtworks.dsl.Dsl.{TryCatch, TryCatchFinally, TryFinally}

import scala.collection._
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.language.implicitConversions
import scala.util.control.NonFatal

final case class Fork[Element](elements: Traversable[Element]) extends AnyVal 

object Fork {
  given [Element]: IsKeyword[Fork[Element], Element] with {}
  private[Fork] object Scala213 {

    @inline
    def flatMapBreakOut[Element, Domain, DomainElement](
        fa: Traversable[Element],
        f: Element => GenTraversableOnce[DomainElement]
    )(implicit factory: Factory[DomainElement, Domain]): Domain = {
      factory.fromSpecific(new View.FlatMap(fa, f))
    }

    @inline
    def newBuilder[A, C](implicit factory: Factory[A, C]): Builder[A, C] = {
      factory.newBuilder
    }

  }

  import Scala213._

  implicit def implicitFork[Element](elements: Traversable[Element]): Fork[Element] = Fork[Element](elements)

  final case class MultipleException(throwableSet: Set[Throwable])
      extends RuntimeException("Multiple exceptions found") {
    override def toString: String = throwableSet.mkString("\n")

    override def printStackTrace(): Unit = {
      for (throwable <- throwableSet) {
        throwable.printStackTrace()
      }
    }

    override def printStackTrace(s: PrintStream): Unit = {
      for (throwable <- throwableSet) {
        throwable.printStackTrace(s)
      }
    }

    override def printStackTrace(s: PrintWriter): Unit = {
      for (throwable <- throwableSet) {
        throwable.printStackTrace(s)
      }
    }

    override def getStackTrace: Array[StackTraceElement] = synchronized {
      super.getStackTrace match {
        case null =>
          setStackTrace(throwableSet.view.flatMap(_.getStackTrace).toArray)
          super.getStackTrace
        case stackTrace =>
          stackTrace
      }
    }

    override def fillInStackTrace(): this.type = {
      this
    }

  }

  implicit def forkContinuationDsl[NarrowElement, LeftDomain, WidenElement, RightDomain](implicit
      eachDsl: Dsl[ForEach[NarrowElement], LeftDomain, NarrowElement],
      booleanEachDsl: Dsl[ForEach[Boolean], LeftDomain, Boolean],
      isTraversableOnce: RightDomain => TraversableOnce[WidenElement],
      canBuildFrom: Factory[WidenElement, RightDomain],
      continueDsl: Dsl[Continue, LeftDomain, Nothing],
      tryCatchFinally: TryCatchFinally[Unit, LeftDomain, LeftDomain, LeftDomain]
  ): Dsl[Fork[NarrowElement], LeftDomain !! RightDomain, NarrowElement] = {
    (fork: Fork[NarrowElement], mapper: NarrowElement => LeftDomain !! RightDomain) =>
      *[[X] =>> LeftDomain !! X] {
        val builder: mutable.Builder[WidenElement, RightDomain] = newBuilder[WidenElement, RightDomain]
        val exceptionBuilder = Set.newBuilder[Throwable]
        val counter = new AtomicInteger(1)
        if (!ForEach(Seq(true, false))) {
          val element = !ForEach(fork.elements)
          counter.incrementAndGet()
          try {
            val result = !Shift(mapper(element))
            builder.synchronized[Unit] {
              builder ++= isTraversableOnce(result)
            }
          } catch {
            case NonFatal(e) =>
              exceptionBuilder.synchronized[Unit] {
                exceptionBuilder += e
              }
          } finally {
            if (counter.decrementAndGet() > 0) {
              !Continue
            }
          }
        } else {
          if (counter.decrementAndGet() > 0) {
            !Continue
          }
        }

        val exceptions = exceptionBuilder.result()
        if (exceptions.isEmpty) {
          builder.result()
        } else {
          val i = exceptions.iterator
          val firstException = i.next()
          if (i.hasNext) {
            throw MultipleException(exceptions)
          } else {
            throw firstException
          }
        }
      }
  }

}
