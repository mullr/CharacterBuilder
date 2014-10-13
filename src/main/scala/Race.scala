import CustomUnitConversions._
import squants.space.LengthConversions._
import squants._
import language.postfixOps


sealed trait Size
case object Small extends Size
case object Medium extends Size
case object Large extends Size

sealed trait Vision
case object Normal extends Vision
case object DarkVision extends Vision
case object BlindSight extends Vision
case object TrueSight extends Vision

case class Race(
  name: String,
  statBonus: StatBonus,
  ageOfAdulthood: Time,
  averageLifespan: Time,
  normalAlignment: Alignment,
  size: Size,
  heightRange: TypedRange[Length],
  averageWeight: Mass,
  baseWalkingSpeed: Velocity,
  vision: Vision,
  rollModifiers: Map[RollType, RollModifier],
  weaponProficiencies: Set[Weapon],
  chooseOneTool: Set[Tool] // TODO: model allowed values for choices that must be made during character creation
)

case class SubRace(
  name: String,
  parentRace: Race,
  statBonus: StatBonus = StatBonus(),
  armorProficiencies: Set[ArmorCategory] = Set())

object Races {
  def dwarf = Race(
    name = "Dwarf",
    statBonus = StatBonus(con = 2),
    ageOfAdulthood = 50 years,
    averageLifespan = 350 years,
    normalAlignment = Alignment(Lawful, Good),
    size = Medium,
    heightRange = TypedRange(4 feet, 5 feet),
    averageWeight = 150 lb,
    baseWalkingSpeed = 25 feetPerRound, //TODO: speed not reduced by heavy armor
    vision = DarkVision,
    rollModifiers = Map( (PoisonSave -> Advantage) ), // TODO: resistance to poison damage
    weaponProficiencies = Set(Weapons.battleaxe, Weapons.handaxe, Weapons.lightHammer, Weapons.warhammer),
    chooseOneTool = Set(Tools.smithsTools, Tools.brewersSupplies, Tools.masonsTools)
    // TODO: stonecunning
  )
}

object SubRaces {
  def hillDwarf = SubRace(
    name = "Hill Dwarf",
    parentRace = Races.dwarf,
    statBonus = StatBonus(wis = 1)
    // TODO: dwarven toughness
  )

  def mountainDwarf = SubRace(
    name = "Mountain Dwarf",
    parentRace = Races.dwarf,
    statBonus = StatBonus(str = 2),
    armorProficiencies = Set(LightArmor, MediumArmor)
  )
}
