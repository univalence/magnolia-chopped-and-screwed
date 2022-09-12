package io.univalence.magnolia_chopped_and_screwed
import io.univalence.magnolia_chopped_and_screwed.Race._

object Naive extends App {

  sealed trait Shout[A] {
    def epicDescription(a: A): String
    def shout(a: A): String
  }

  val humanShoutAndDescribe: Shout[Human] =
    new Shout[Human] {
      override def epicDescription(a: Human): String =
        s"""
           |FIRSTNAME              : ${a.firstName}
           |LASTNAME               : ${a.lastName}
           |SOUND                  : ${a.sound}
           |SOCIAL SECURITY NUMBER : ${a.socialSecurityNumber}
           |""".stripMargin
      override def shout(a: Human): String = s"${a.firstName} ${a.lastName} shouts: ${a.sound.toUpperCase}"
    }

  implicit class HumanOps(human: Human) {
    def shout: String           = humanShoutAndDescribe.shout(human)
    def epicDescription: String = humanShoutAndDescribe.epicDescription(human)
  }

  val johnDoe = Human(firstName = "John", lastName = "Doe", sound = "Hello", socialSecurityNumber = 123)

  println(johnDoe.shout)
  println(johnDoe.epicDescription)
}
