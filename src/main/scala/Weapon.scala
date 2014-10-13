import CustomUnitConversions._
import squants._
import language.postfixOps

sealed trait DamageType
case object Bludgeoning extends DamageType
case object Piercing extends DamageType
case object Slashing extends DamageType

sealed trait WeaponCombatType
case object Melee extends WeaponCombatType
case object Ranged extends WeaponCombatType

sealed trait WeaponTrainingType
case object Simple extends WeaponTrainingType
case object Martial extends WeaponTrainingType


case class Weapon (
  name: String,
  cost: Money,
  trainingType: WeaponTrainingType,
  combatType: WeaponCombatType,
  damageRoll: Roll,
  damageType: DamageType,
  weight: Mass,
  rollModifiers: Map[RollType, RollModifier] = Map(),
  requiredStats: StatBlock = StatBlock.zero) {
  // todo: other weapon properties

  def apply(s: Sheet) = {
    val attack = Attack(weapon = this)
    s.copy(attacks = s.attacks + attack)
  }
}


object Weapons {
  val club           = Weapon("Club",           1 sp,  Simple, Melee,  Roll(1, d4), Bludgeoning,  2 lb)
  val dagger         = Weapon("Dagger",         2 gp,  Simple, Melee,  Roll(1, d4), Piercing,     1 lb)
  val greatclub      = Weapon("Greatclub",      2 sp,  Simple, Melee,  Roll(1, d8), Bludgeoning, 10 lb)
  val handaxe        = Weapon("Handaxe",        5 gp,  Simple, Melee,  Roll(1, d6), Slashing,     2 lb)
  val javelin        = Weapon("Javelin",        5 sp,  Simple, Melee,  Roll(1, d6), Piercing,     2 lb)
  val lightHammer    = Weapon("Light hammer",   2 gp,  Simple, Melee,  Roll(1, d4), Bludgeoning,  2 lb)
  val mace           = Weapon("Mace",           5 gp,  Simple, Melee,  Roll(1, d6), Bludgeoning,  4 lb)
  val quarterstaff   = Weapon("Quarterstaff",   2 sp,  Simple, Melee,  Roll(1, d6), Bludgeoning,  4 lb)
  val sickle         = Weapon("Sickle",         1 gp,  Simple, Melee,  Roll(1, d4), Slashing,     2 lb)
  val spear          = Weapon("Spear",          1 gp,  Simple, Melee,  Roll(1, d6), Piercing,     3 lb)

  val lightCrossbow  = Weapon("Light crossbow", 25 gp, Simple, Ranged, Roll(1, d8), Piercing,     5 lb)
  val dart           = Weapon("Dart",           5 cp,  Simple, Ranged, Roll(1, d4), Piercing,     0.25 lb)
  val shortbow       = Weapon("Shortbow",       25 gp, Simple, Ranged, Roll(1, d6), Piercing,     2 lb)
  val sling          = Weapon("Shortbow",       1 sp,  Simple, Ranged, Roll(1, d4), Bludgeoning,  0 lb)

  val battleaxe      = Weapon("Battleaxe",      10 gp, Martial, Melee, Roll(1, d8), Slashing,     4 lb)
  val warhammer      = Weapon("Warhammer",      15 gp, Martial, Melee, Roll(1, d8), Bludgeoning,  2 lb)
}
