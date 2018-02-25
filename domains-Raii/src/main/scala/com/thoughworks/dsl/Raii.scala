package com.thoughworks.dsl

/**
  * @author 杨博 (Yang Bo)
  */
trait Raii[OtherDomain, +Resource] {
  def using(handler: Resource => OtherDomain): OtherDomain
}

object Raii {
  def move[OtherDomain, Resource](resource: Resource): Raii[OtherDomain, Resource] = new Raii[OtherDomain, Resource] {
    def using(handler: Resource => OtherDomain): OtherDomain = handler(resource)
  }

}
