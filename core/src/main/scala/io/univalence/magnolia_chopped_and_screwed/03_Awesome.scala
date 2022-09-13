package io.univalence.magnolia_chopped_and_screwed
import magnolia1.{CaseClass, Magnolia, SealedTrait}

import io.univalence.magnolia_chopped_and_screwed.Race._

object Awesome extends App {

  sealed trait Shout[A] {
    def shout(a: A): String
  }
  trait ShoutCC[A] extends Shout[A] {
    def epicDescription(a: A): String
  }
  trait ShoutValue[A] extends Shout[A]

  implicit val shoutString: ShoutValue[String] = str => str
  implicit val shoutInt: ShoutValue[Int]       = i => i.toString

  implicit class ShoutCCOps[A <: AnyRef with Product](a: A)(implicit shoutCC: ShoutCC[A]) {
    def shout: String           = shoutCC.shout(a)
    def epicDescription: String = shoutCC.epicDescription(a)
  }

  val human  = Human(firstName = "John", lastName = "Doe", sound = "Hello world", socialSecurityNumber = 123)
  val dwarf  = Dwarf(name = "gimli", sound = "uh", goldAmount = 1000)
  val hybrid = GenericHybrid(human, dwarf)

  implicit val shoutRace: Shout[Race] = AwesomeShoutDerivation.bugGenR[Race]
  println(AwesomeShoutDerivation.gen[GenericHybrid].epicDescription(hybrid))

}
object AwesomeShoutDerivation {
  import Awesome._

  implicit def gen[T]: ShoutCC[T] = macro Magnolia.gen[T]

  def bugGenR[T]: Shout[T] = macro Magnolia.gen[T]

  type Typeclass[T] = Shout[T]

  def split[T](ctx: SealedTrait[Shout, T]): ShoutCC[T] =
    new ShoutCC[T] {
      override def shout(a: T): String =
        ctx.split(a) { sub =>
          sub.typeclass.asInstanceOf[ShoutCC[sub.SType]].shout(sub.cast(a))
        }

      override def epicDescription(a: T): String =
        ctx.split(a) { sub =>
          sub.typeclass.asInstanceOf[ShoutCC[sub.SType]].epicDescription(sub.cast(a))
        }
    }

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
