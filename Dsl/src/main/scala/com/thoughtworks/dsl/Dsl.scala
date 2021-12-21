package com.thoughtworks.dsl
import com.thoughtworks.dsl.Dsl.!!

import scala.annotation._
import scala.collection._
import scala.collection.mutable.Builder
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import scala.util.NotGiven
import scala.util.control.Exception.Catcher
import scala.util.{Failure, Success, Try}
import scala.util.control.{NonFatal, TailCalls}
import scala.util.control.TailCalls.TailRec

/** The domain-specific interpreter for `Keyword` in `Domain`, which is a dependent type type class that registers an
  * asynchronous callback function, to handle the `Value` inside `Keyword`.
  *
  * @tparam Value
  *   The value held inside `Keyword`.
  * @author
  *   杨博 (Yang Bo)
  * @example
  *   Creating a collaborative DSL in [[https://github.com/ThoughtWorksInc/Dsl.scala Dsl.scala]] is easy. Only two steps
  *   are required:
  *
  *   - Defining their domain-specific [[com.thoughtworks.dsl.Dsl.Keyword Keyword]].
  *   - Implementing this [[Dsl]] type class, which is an interpreter for an
  *     [[com.thoughtworks.dsl.Dsl.Keyword Keyword]].
  */
@implicitNotFound("The keyword:\n ${Keyword}\nis not supported inside a function that returns:\n${Domain}.")
trait Dsl[-Keyword, Domain, +Value]:
  /** Registers an asynchronous callback `handler` on `keyword`, to handle the `Value`. */
  def cpsApply(keyword: Keyword, handler: Value => Domain): Domain


private[dsl] trait LowPriorityDsl1 { this: Dsl.type =>

  given deriveFunction1Dsl[Keyword, FunctionDomain, State, Domain, Value](using
      isFunctionDomain: FunctionDomain =:= (State => Domain),
      restDsl: Dsl.Atomic[Keyword, Domain, Value]
  ): Dsl.Atomic[Keyword, FunctionDomain, Value] = {
    isFunctionDomain.substituteContra[[X] =>> Dsl.Atomic[Keyword, X, Value]] {
      (keyword: Keyword, handler: Value => State => Domain) =>
        val restDsl1 = restDsl
        locally { (state: State) =>
          val handler1 = handler
          restDsl1.cpsApply(keyword, handler1(_)(state))
        }
    }
  }

}

private[dsl] trait LowPriorityDsl0 extends LowPriorityDsl1 { this: Dsl.type =>

//  // FIXME: Shift
//  implicit def continuationDsl[Keyword, LeftDomain, RightDomain, Value](
//      implicit restDsl: Dsl.Atomic[Keyword, LeftDomain, Value],
//      shiftDsl2: Dsl.Atomic[Shift[LeftDomain, RightDomain], LeftDomain, RightDomain]
//  ): Dsl.Atomic[Keyword, LeftDomain !! RightDomain, Value] = {
//    new Dsl.Atomic[Keyword, LeftDomain !! RightDomain, Value] {
//      def cpsApply(keyword: Keyword, handler: Value => LeftDomain !! RightDomain): LeftDomain !! RightDomain = {
//        (continue: RightDomain => LeftDomain) =>
//          restDsl.cpsApply(keyword, { a =>
//            restDsl2.cpsApply(handler(a), continue)
//          })
//      }
//    }
//  }

  implicit def throwableContinuationDsl[Keyword, ThrowableContinuationDomain, LeftDomain, Value](implicit
      isThrowableContinuationDomain: ThrowableContinuationDomain =:= (LeftDomain !! Throwable),
      restDsl: Dsl.Atomic[Keyword, LeftDomain, Value]
  ): Dsl.Atomic[Keyword, ThrowableContinuationDomain, Value] =
    isThrowableContinuationDomain.substituteContra[[X] =>> Dsl.Atomic[Keyword, X, Value]] { (keyword, handler) => continue =>
      restDsl.cpsApply(
        keyword,
        new (Value => LeftDomain) {
          def apply(value: Value): LeftDomain = {
            val protectedContinuation =
              try {
                handler(value)
              } catch {
                case NonFatal(e) =>
                  return continue(e)
              }
            // FIXME: Shift[Domain, Throwable]
            protectedContinuation(continue)
          }
        }
      )
    }

}

object Dsl extends LowPriorityDsl0 {

  trait Derived[-Keyword, Domain, +Value] extends Dsl[Keyword, Domain, Value]
  trait Composed[-Keyword, Domain, +Value] extends Dsl[Keyword, Domain, Value]
  trait Atomic[-Keyword, Domain, +Value] extends Dsl[Keyword, Domain, Value]

  opaque type Searching[-Keyword, Domain, +Value] <: Dsl[Keyword, Domain, Value] = Dsl[Keyword, Domain, Value]
  object Searching extends Searching.AtomicThenComposedThenDerived:
    private[Searching] trait Derived:
      given [Keyword, Domain, Value](using dsl: Dsl.Derived[Keyword, Domain, Value]): Dsl.Searching[Keyword, Domain, Value] = dsl
    private[Searching] trait ComposedThenDerived extends Searching.Derived:
      given [Keyword, Domain, Value](using dsl: Dsl.Composed[Keyword, Domain, Value]): Dsl.Searching[Keyword, Domain, Value] = dsl
    private[Searching] trait AtomicThenComposedThenDerived extends Searching.ComposedThenDerived:
      given [Keyword, Domain, Value](using dsl: Dsl.Atomic[Keyword, Domain, Value]): Dsl.Searching[Keyword, Domain, Value] = dsl
    object AtomicThenComposedThenDerived extends AtomicThenComposedThenDerived

    private[Searching] trait Composed:
      given [Keyword, Domain, Value](using dsl: Dsl.Composed[Keyword, Domain, Value]): Dsl.Searching[Keyword, Domain, Value] = dsl
    private[Searching] trait DerivedThenComposed extends Searching.Composed:
      given [Keyword, Domain, Value](using dsl: Dsl.Derived[Keyword, Domain, Value]): Dsl.Searching[Keyword, Domain, Value] = dsl
    private[Searching] trait AtomicThenDerivedThenComposed extends Searching.DerivedThenComposed:
      given [Keyword, Domain, Value](using dsl: Dsl.Atomic[Keyword, Domain, Value]): Dsl.Searching[Keyword, Domain, Value] = dsl
    object AtomicThenDerivedThenComposed extends AtomicThenDerivedThenComposed


  extension [Keyword, Value](inline from: Keyword)(using inline asKeyword: Dsl.IsKeyword[Keyword, Value])
    transparent inline def unary_! : Value = {
      Dsl.shift[Keyword, Value](from)
    }

  sealed trait HasValueOrElement[KeywordOrView, Element]:
    extension (keywordOrView: KeywordOrView)
      def flatMap[Mapped <: For.Yield[MappedElement], MappedElement](flatMapper: Element => Mapped) = For.Yield.FlatMap(keywordOrView, flatMapper)
      def map[Mapped](mapper: Element => Mapped) = For.Yield.Map(keywordOrView, mapper)
      def foreach[Nested <: For.Do](action: Element => Nested) = For.Do.FlatForeach(keywordOrView, action)
      def foreach(action: Element => Unit) = For.Do.Foreach(keywordOrView, action)
      def withFilter(filter: Element => Boolean) = For.Yield.WithFilter(keywordOrView, filter)
      // TODO: Implement `foreach` and `map` in macros to support !-notation in `do` block or `yield` block
  object HasValueOrElement {
    given [KeywordOrView <: For.Yield[Element], Element]: HasValueOrElement[KeywordOrView, Element] with {}
  }

  /** The AST returned from a `for`...`yield` or a `for`...`do` expression.
    *
    * Note that a [[For]] does not directly support !-notation.
    * Instead, [[keywords.Each.ToView]] is used to convert a [[For]] to a
    * [[Keyword]] that supports !-notation.
    */
  sealed trait For
  object For {
    /** The AST returned from a `for`...`do` expression. */
    sealed trait Do extends For
    object Do {
      final case class KeywordForeach[Upstream, UpstreamElement, UnitKeyword](upstream: Upstream, action: UpstreamElement => UnitKeyword) extends Do
      final case class Foreach[Upstream, UpstreamElement](upstream: Upstream, action: UpstreamElement => Unit) extends Do
      final case class FlatForeach[Upstream, UpstreamElement, Nested <: Do](upstream: Upstream, action: UpstreamElement => Nested) extends Do
    }
    /** The AST returned from a `for`...`yield` expression. */
    sealed trait Yield[Element] extends For
    object Yield {
      final case class KeywordMap[Upstream, UpstreamElement, ElementKeyword, Element](upstream: Upstream, mapper: UpstreamElement => ElementKeyword) extends Yield[Element]
      final case class Map[Upstream, UpstreamElement, Element](upstream: Upstream, mapper: UpstreamElement => Element) extends Yield[Element]
      final case class FlatMap[Upstream, UpstreamElement, Mapped <: Yield[Element], Element](upstream: Upstream, flatMapper: UpstreamElement => Mapped) extends Yield[Element]
      final case class WithFilter[Upstream, Element](upstream: Upstream, filter: Element => Boolean) extends Yield[Element]
    }
  }

  implicit def derivedTailRecDsl[Keyword, TailRecDomain, Domain, Value](implicit
      isTailRecDomain: TailRecDomain =:= TailRec[Domain],
      restDsl: Dsl.Atomic[Keyword, Domain, Value]
  ): Dsl.Atomic[Keyword, TailRecDomain, Value] = isTailRecDomain.substituteContra[[X] =>> Dsl.Atomic[Keyword, X, Value]] {
    (keyword, handler) =>
      TailCalls.done {
        restDsl.cpsApply(
          keyword,
          { value =>
            handler(value).result
          }
        )
      }
  }

  implicit def derivedThrowableTailRecDsl[Keyword, TaskDomain, LeftDomain, Value](implicit
      isTaskDomain: TaskDomain =:= (TailRec[LeftDomain] !! Throwable),
      restDsl: Dsl.Atomic[Keyword, LeftDomain !! Throwable, Value]
  ): Dsl.Atomic[Keyword, TaskDomain, Value] = isTaskDomain.substituteContra[[X] =>> Dsl.Atomic[Keyword, X, Value]] {
    (keyword, handler) => tailRecFailureHandler =>
      TailCalls.done(
        restDsl.cpsApply(
          keyword,
          { value => failureHandler =>
            handler(value) { e =>
              TailCalls.done(failureHandler(e))
            }.result
          }
        ) { e =>
          tailRecFailureHandler(e).result
        }
      )
  }

  private[dsl] type !![R, +A] = (A => R) => R

  private def catchNativeException[A](futureContinuation: Future[A] !! A): Future[A] = {
    try {
      futureContinuation(Future.successful)
    } catch {
      case NonFatal(e) =>
        Future.failed(e)
    }
  }

  /** The type class to support `try` ... `catch` ... `finally` expression for `OutputDomain`.
    *
    * !-notation is allowed by default for `? !! Throwable` and [[scala.concurrent.Future Future]] domains, with the
    * help of this type class.
    */
  @implicitNotFound(
    "The `try` ... `catch` ... `finally` expression cannot contain !-notation inside a function that returns ${OuterDomain}."
  )
  trait TryCatchFinally[Value, OuterDomain, BlockDomain, FinalizerDomain] {
    def tryCatchFinally(
        block: BlockDomain !! Value,
        catcher: Catcher[BlockDomain !! Value],
        finalizer: FinalizerDomain !! Unit,
        outerSuccessHandler: Value => OuterDomain
    ): OuterDomain
  }

  object TryCatchFinally {

    implicit def fromTryCatchTryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain](implicit
        tryFinally: TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain],
        tryCatch: TryCatch[Value, BlockDomain, BlockDomain]
    ): TryCatchFinally[Value, OuterDomain, BlockDomain, FinalizerDomain] = {
      (
          block: BlockDomain !! Value,
          catcher: Catcher[BlockDomain !! Value],
          finalizer: FinalizerDomain !! Unit,
          outerSuccessHandler: Value => OuterDomain
      ) =>
        tryFinally.tryFinally(
          {
            tryCatch.tryCatch(block, catcher, _)
          },
          finalizer,
          outerSuccessHandler
        )
    }
  }

  @implicitNotFound(
    "The `try` ... `catch` expression cannot contain !-notation inside a function that returns ${OuterDomain}."
  )
  trait TryCatch[Value, OuterDomain, BlockDomain] {
    def tryCatch(
        block: BlockDomain !! Value,
        catcher: Catcher[BlockDomain !! Value],
        outerSuccessHandler: Value => OuterDomain
    ): OuterDomain
  }

  private[dsl] trait LowPriorityTryCatch {
    implicit def liftFunction1TryCatch[Value, OuterDomain, BlockDomain, State](implicit
        restTryCatch: TryCatch[Value, OuterDomain, BlockDomain]
    ): TryCatch[Value, State => OuterDomain, State => BlockDomain] = {
      (
          block: (State => BlockDomain) !! Value,
          catcher: Catcher[(State => BlockDomain) !! Value],
          outerSuccessHandler: Value => State => OuterDomain
      ) => (state: State) =>
        def withState(blockContinuation: (State => BlockDomain) !! Value) = { (blockHandler: (Value => BlockDomain)) =>
          blockContinuation { (value: Value) => (state: State) =>
            blockHandler(value)
          }(state)
        }

        restTryCatch.tryCatch(withState(block), catcher.andThen(withState _), outerSuccessHandler(_)(state))
    }
  }

  object TryCatch extends LowPriorityTryCatch {

    implicit def throwableContinuationTryCatch[LeftDomain, Value]
        : TryCatch[Value, LeftDomain !! Throwable, LeftDomain !! Throwable] = {

      (
          block: LeftDomain !! Throwable !! Value,
          catcher: Catcher[LeftDomain !! Throwable !! Value],
          outerSuccessHandler: Value => LeftDomain !! Throwable
      ) => outerFailureHandler =>
        def innerFailureHandler(e: Throwable): LeftDomain = {
          catcher.lift(e) match {
            case None =>
              outerFailureHandler(e)
            case Some(recovered) =>
              @inline
              def recoveredHandler(): LeftDomain = {
                locally {
                  try {
                    recovered(outerSuccessHandler)
                  } catch {
                    case NonFatal(nativeThrown) =>
                      return outerFailureHandler(nativeThrown)
                  }
                }(outerFailureHandler)
              }

              recoveredHandler()
          }
        }

        def runBlock(): LeftDomain = {
          (try {
            block { a => hookedFailureHandler =>
              @inline
              def successHandler(): LeftDomain = {
                locally {
                  try {
                    outerSuccessHandler(a)
                  } catch {
                    case NonFatal(nativeThrown) =>
                      return outerFailureHandler(nativeThrown)
                  }
                }(outerFailureHandler)
              }

              successHandler()
            }
          } catch {
            case NonFatal(e) =>
              return innerFailureHandler(e)
          })(innerFailureHandler)
        }
        runBlock()
    }

    implicit def futureTryCatch[BlockValue, OuterValue](implicit
        executionContext: ExecutionContext
    ): TryCatch[BlockValue, Future[OuterValue], Future[BlockValue]] = {
      (
          block: Future[BlockValue] !! BlockValue,
          catcher: Catcher[Future[BlockValue] !! BlockValue],
          outerSuccessHandler: BlockValue => Future[OuterValue]
      ) =>
        catchNativeException(block)
          .recoverWith { case e: Throwable =>
            def recover(): Future[BlockValue] = {
              (try {
                catcher.lift(e)
              } catch {
                case NonFatal(extractorException) =>
                  return Future.failed(extractorException)
              }) match {
                case None =>
                  Future.failed(e)
                case Some(recovered) =>
                  catchNativeException(recovered)
              }
            }
            recover()
          }
          .flatMap(outerSuccessHandler)
    }
  }

  @implicitNotFound(
    "The `try` ... `finally` expression cannot contain !-notation inside a function that returns ${OuterDomain}."
  )
  trait TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain] {
    def tryFinally(
        block: BlockDomain !! Value,
        finalizer: FinalizerDomain !! Unit,
        outerSuccessHandler: Value => OuterDomain
    ): OuterDomain
  }

  private[dsl] trait LowPriorityTryFinally {
    implicit def liftFunction1TryCatch[Value, OuterDomain, BlockDomain, FinalizerDomain, State](implicit
        restTryFinally: TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain]
    ): TryFinally[Value, State => OuterDomain, State => BlockDomain, State => FinalizerDomain] = {
      (
          block: (State => BlockDomain) !! Value,
          finalizer: (State => FinalizerDomain) !! Unit,
          outerSuccessHandler: Value => State => OuterDomain
      ) => state =>
        def withState[Domain, Value](blockContinuation: (State => Domain) !! Value) = {
          (blockHandler: (Value => Domain)) =>
            blockContinuation { (value: Value) => (state: State) =>
              blockHandler(value)
            }(state)
        }

        restTryFinally.tryFinally(withState(block), withState(finalizer), outerSuccessHandler(_)(state))
    }
  }

  object TryFinally extends LowPriorityTryFinally {

    implicit def futureTryFinally[BlockValue, OuterValue](implicit
        executionContext: ExecutionContext
    ): TryFinally[BlockValue, Future[OuterValue], Future[BlockValue], Future[Unit]] = {
      (
          block: Future[BlockValue] !! BlockValue,
          finalizer: Future[Unit] !! Unit,
          outerSuccessHandler: BlockValue => Future[OuterValue]
      ) =>
        @inline
        def injectFinalizer[A](f: Unit => Future[A]): Future[A] = {
          catchNativeException(finalizer).flatMap(f)
        }

        catchNativeException(block)
          .recoverWith { case e: Throwable =>
            injectFinalizer { (_: Unit) =>
              Future.failed(e)
            }
          }
          .flatMap { a =>
            injectFinalizer { (_: Unit) =>
              outerSuccessHandler(a)
            }
          }
    }

    implicit def throwableContinuationTryFinally[LeftDomain, Value]
        : TryFinally[Value, LeftDomain !! Throwable, LeftDomain !! Throwable, LeftDomain !! Throwable] = {
      (block, finalizer, outerSuccessHandler) => outerFailureHandler =>
        @inline
        def injectFinalizer(finalizerHandler: Unit => LeftDomain !! Throwable): LeftDomain = {
          locally {
            try {
              finalizer(finalizerHandler)
            } catch {
              case NonFatal(e) =>
                return outerFailureHandler(e)
            }
          }(outerFailureHandler)
        }

        @inline
        def hookedFailureHandler(e: Throwable) =
          injectFinalizer { (_: Unit) =>
            _(e)
          }

        def runBlock(): LeftDomain = {
          (try {
            block { value => hookedFailureHandler =>
              injectFinalizer { (_: Unit) =>
                outerSuccessHandler(value)
              }
            }
          } catch {
            case NonFatal(e) =>
              return hookedFailureHandler(e)
          })(hookedFailureHandler)
        }
        runBlock()
    }
  }

  @FunctionalInterface
  trait Lift[From, +To] extends (From => To)
  private[dsl] trait LowPriorityLift0 { this: Lift.type =>

    given [From, Intermediate, To](using
        step1: OneStep[Intermediate, To],
        step0: Lift[From, Intermediate]
    ): Lift[From, To] with {
      def apply(from: From): To = {
        step1(step0(from))
      }
    }

  }
  object Lift extends LowPriorityLift0 {
    @FunctionalInterface
    trait OneStep[From, +To] extends Lift[From, To]

    given [CastFrom, CastTo >: CastFrom]: Lift[CastFrom, CastTo] with {
      def apply(from: CastFrom): CastTo = {
        from
      }
    }

    private[Lift] trait LowPriorityOneStep1 { this: OneStep.type =>
      given [Collection, Element](using
          factory: collection.Factory[Element, Collection]
      ): OneStep[Element, Collection] = { element =>
        factory.fromSpecific(element :: Nil)
      }
    }

    private[Lift] trait LowPriorityOneStep0 extends LowPriorityOneStep1 { this: OneStep.type =>
      given [R, F, A]: OneStep[R, A => R] = { r => Function.const(r) }
    }

    object OneStep extends LowPriorityOneStep0 {

      import Dsl.!!
      given [LeftDomain, RightDomain]: OneStep[RightDomain, LeftDomain !! RightDomain] = r => _(r)

      given [Element](using
          ExecutionContext
      ): OneStep[Element, Future[Element]] = {
        Future.successful
      }
    }

  }

  opaque type Run[Keyword, Domain, Value] <: Keyword => Domain =
    Keyword => Domain

  object Run {

    given [Keyword, Domain, Value](using
        dsl: /*=>*/ Dsl.Searching[Keyword, Domain, Value],
        lift: /*=>*/ Lift[Value, Domain]
    ): Run[Keyword, Domain, Value] = { dsl.cpsApply(_, lift) }

  }

  type Keyword = Keyword.Opaque | Keyword.Trait
  object Keyword {
    /** A marker trait that denotes a keyword class, enabling extension method
      * defined in [[Dsl]] for subclasses of [[Keyword.Trait]].
      */
    trait Trait extends Any

    /** A marker trait that denotes a keyword opaque type, enabling extension
      * method defined in [[Dsl]] for its subtypes of [[Keyword.Opaque]].
      */
    opaque type Opaque = Any
    object Opaque {
      opaque type Of[+Self] <: Self & Opaque = Self
      object Of {
        def apply[Self]: Self =:= Of[Self] = summon
      }
    }
  }
  
 
  trait IsKeyword[Keyword, Value] extends HasValueOrElement[Keyword, Value]:
    extension (keyword: Keyword)
      @inline def to[Domain[_]](using
          run: Run[Keyword, Domain[Value], Value]
      ): Domain[Value] = {
        run(keyword)
      }

      @inline def as[Domain](using
          run: Run[Keyword, Domain, Value]
      ): Domain = {
        run(keyword)
      }


  extension [Keyword, Domain, Value](keyword: Keyword)
    @inline def cpsApply(using
        dsl: Dsl.Searching[Keyword, Domain, Value]
    )(handler: Value => Domain)(using DummyImplicit): Domain = {
      dsl.cpsApply(keyword, handler)
    }

  @annotation.compileTimeOnly("""This method must be called only inside a `reset` or `*` code block.""")
  def shift[Keyword, Value](keyword: Keyword): Value = ???

}
