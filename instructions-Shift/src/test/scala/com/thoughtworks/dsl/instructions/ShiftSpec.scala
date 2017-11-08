package com.thoughtworks.dsl.instructions
import org.scalatest._

import scala.annotation.tailrec
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
final class ShiftSpec extends FreeSpec with Matchers {
  import ShiftSpec._



  "while" in {

    def server: Server = {
      while (!Shift[Server, Boolean] { (handler: Boolean => Server) =>
               {
                 case "break" =>
                   "[while condition] break..." -> handler(false)
                 case request =>
                   s"[while condition] $request ignored; continue" -> handler(true)
               }
             }) {

        !Shift[Server, Unit] { (handler: Unit => Server) => request: Request =>
          s"[while body] $request ignored" -> handler(())
        }

      }

      object Rest extends Server {
        def call(request: Request): (Response, Server) = {
          s"[rest] $request ignored" -> this
        }
      }

      Rest

    }
  }
}

object ShiftSpec {
//  sealed trait Sink[-A]
//  case object End extends Sink[Any]
//  case class Write[-A](f: A => Sink[A]) extends Sink[A]
//
//type MySink[A] = (A => Option[T] )forSome {type T <: MySink[A]}
//  type Sink[A] = T forSome { type T <: A => T }

//  trait Foo {
  //}

  type Request = String
  type Response = String
  trait Server {
    def call(request: Request): (Response, Server)
  }

}
