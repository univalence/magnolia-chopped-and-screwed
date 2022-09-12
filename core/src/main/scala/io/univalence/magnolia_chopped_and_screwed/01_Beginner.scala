package io.univalence.magnolia_chopped_and_screwed

import magnolia1.{CaseClass, Magnolia}

import io.univalence.magnolia_chopped_and_screwed.Beginner.Shout
import io.univalence.magnolia_chopped_and_screwed.Race._

object Beginner extends App {
  sealed trait Shout[A] {
    def epicDescription(a: A): String
    def shout(a: A): String
  }

  implicit val shoutString: Shout[String] =
    new Shout[String] {
      override def epicDescription(a: String): String = ???
      override def shout(a: String): String           = a
    }

  implicit val shoutInt: Shout[Int] =
    new Shout[Int] {
      override def epicDescription(a: Int): String = ???
      override def shout(a: Int): String           = a.toString
    }

  val shoutingHuman: Shout[Human] = ShoutDerivation.gen[Human]

  val human: Human = Human(firstName = "John", lastName = "Doe", sound = "Hello world", socialSecurityNumber = 123)
  println(shoutingHuman.shout(human))

}

object ShoutDerivation {
  type Typeclass[T] = Shout[T]

  def join[T](ctx: CaseClass[Shout, T]): Shout[T] =
    new Shout[T] {
      override def epicDescription(a: T): String = ???

      override def shout(a: T): String =
        ctx.parameters.find(_.label == "sound") match {
          case Some(value) => value.typeclass.shout(value.dereference(a)).toUpperCase
          case None        => "I can't shout"
        }
    }

  implicit def gen[T]: Shout[T] = macro Magnolia.gen[T]
}
