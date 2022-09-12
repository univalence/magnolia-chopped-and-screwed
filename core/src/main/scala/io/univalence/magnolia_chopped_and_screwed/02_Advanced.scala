package io.univalence.magnolia_chopped_and_screwed

import magnolia1.{CaseClass, Magnolia}

import io.univalence.magnolia_chopped_and_screwed.AdvancedShoutDerivation._
import io.univalence.magnolia_chopped_and_screwed.Race._

object Advanced extends App {

  sealed trait Shout[A] {
    def shout(a: A): String
  }
  object Shout {
    def shout[A: Shout](a: A): String = implicitly[Shout[A]].shout(a)
  }

  trait ShoutCC[A] extends Shout[A] {
    def epicDescription(a: A): String
  }

  object ShoutCC {
    def epicDescription[A: ShoutCC](a: A): String = implicitly[ShoutCC[A]].epicDescription(a)
  }

  trait ShoutValue[A] extends Shout[A]

  implicit val shoutString: ShoutValue[String] = str => str.toUpperCase
  implicit val shoutInt: ShoutValue[Int]       = i => i.toString

  implicit class ShoutCCOps[A <: AnyRef with Product](a: A)(implicit shoutCC: ShoutCC[A]) {
    def shout: String           = shoutCC.shout(a)
    def epicDescription: String = shoutCC.epicDescription(a)
  }

  val human        = Human(firstName = "John", lastName = "Doe", sound = "Hello world", socialSecurityNumber = 123)
  val elf          = Elf(name = "test", skincareRoutine = "ok")
  val dwarf        = Dwarf(name = "gimli", sound = "uh", goldAmount = 1000)
  val littlePerson = LittlePerson(human, dwarf)

  println(AdvancedShoutDerivation.gen[Human].shout(human))
  println(littlePerson.epicDescription)

}

object AdvancedShoutDerivation {
  import Advanced._
  implicit def gen[T]: ShoutCC[T] = macro Magnolia.gen[T]

  type Typeclass[T] = Shout[T]

  def join[T](ctx: CaseClass[Shout, T]): ShoutCC[T] =
    new ShoutCC[T] {
      override def epicDescription(a: T): String =
        ctx.parameters
          .map(param =>
            param.typeclass match {
              case cc: ShoutCC[param.PType] => s"${param.label} part: [${cc.epicDescription(param.dereference(a))}] \n"
              case cc: ShoutValue[param.PType] => s"${param.label} = ${cc.shout(param.dereference(a))} \n"
            }
          )
          .mkString

      override def shout(a: T): String =
        ctx.parameters.find(_.label == "sound") match {
          case Some(value) => value.typeclass.shout(value.dereference(a)).toUpperCase
          case None        => "I can't shout"
        }
    }

}
