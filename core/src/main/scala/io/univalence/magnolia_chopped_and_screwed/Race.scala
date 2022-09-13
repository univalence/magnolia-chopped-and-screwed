package io.univalence.magnolia_chopped_and_screwed

object Race {
  sealed trait Race

  // Beginner
  case class Human(firstName: String, lastName: String, sound: String, socialSecurityNumber: Int) extends Race
  case class Dwarf(name: String, sound: String, goldAmount: Int)                                  extends Race
  case class Elf(name: String, skincareRoutine: String)                                           extends Race

  // Advanced
  case class LittlePerson(human: Human, dwarf: Dwarf) extends Race

  // Awesome
  case class GenericHybrid(race1: Race, race2: Race) extends Race
  case object Blob                                   extends Race
}
