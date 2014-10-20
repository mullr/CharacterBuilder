import squants.Mass

case class TypedRange[T](min: T, max: T)

case class StatBlock(str: Int=10, dex: Int=10, con: Int=10, int: Int=10, wis: Int=10, cha: Int=10) {
  def +(bonus: StatBonus) =
    StatBlock(str + bonus.str, dex + bonus.dex, con + bonus.con, int + bonus.int, wis + bonus.wis, cha + bonus.cha)
}

object StatBlock {
  val zero = StatBlock(0, 0, 0, 0, 0, 0)
}

case class StatBonus(str: Int=0, dex: Int=0, con: Int=0, int: Int=0, wis: Int=0, cha: Int=0)

sealed trait SocialAlignment
case object Lawful extends SocialAlignment
case object SocialNeutral extends SocialAlignment
case object Chaotic extends SocialAlignment

sealed trait OrderAlignment
case object Good extends OrderAlignment
case object OrderNeutral extends OrderAlignment
case object Evil extends SocialAlignment

case class Alignment(social: SocialAlignment, order: OrderAlignment)

trait Item {
  def name: String
  def weight: Mass
  def cost: Money
  def rollModifiers: Map[RollType, RollModifier]
  def requiredStats: StatBlock
}

case class Tool(name: String)
object Tools {
  val smithsTools = Tool("Smith's tools")
  val brewersSupplies = Tool("Brewer's supplies")
  val masonsTools = Tool("Mason's tools")
}

case class Language(name: String)
object Languages {
  val common = Language("Common")
  val dwarvish = Language("Dwarvish")
}

case class CharacterParameter[A](
  numChoices: Int,
  choices: Set[A]
)

sealed trait Stat
case object Str extends Stat
case object Dex extends Stat
case object Con extends Stat
case object Int extends Stat
case object Wis extends Stat
case object Cha extends Stat


sealed trait Skill { val baseStat: Stat }
case object Acrobatics     extends Skill { val baseStat = Dex }
case object AnimalHandling extends Skill { val baseStat = Wis }
case object Arcana         extends Skill { val baseStat = Int }
case object Athletics      extends Skill { val baseStat = Str }
case object Deception      extends Skill { val baseStat = Cha }
case object History        extends Skill { val baseStat = Int }
case object Insight        extends Skill { val baseStat = Wis }
case object Intimidation   extends Skill { val baseStat = Cha }
case object Investigation  extends Skill { val baseStat = Int }
case object Medicine       extends Skill { val baseStat = Wis }
case object Nature         extends Skill { val baseStat = Int }
case object Perception     extends Skill { val baseStat = Wis }
case object Performance    extends Skill { val baseStat = Cha }
case object Persuasion     extends Skill { val baseStat = Cha }
case object SleightOfHand  extends Skill { val baseStat = Dex }
case object Stealth        extends Skill { val baseStat = Dex }
case object Survival       extends Skill { val baseStat = Wis }

sealed trait ActionType
case object MoveAction
case object AttackAction
case object BonusAction
case object ReactionAction

sealed trait AbilityReuse
case object AtWill
case class RechargeAfterShortRest(uses: Int)
case class RechargeAfterLongRest(uses: Int)

case class WeaponProficiency (
  trainingType: WeaponTrainingType,
  combatType: WeaponCombatType
)

// For convenience
object WeaponProficiency {
  def simpleMelee   = WeaponProficiency(Simple,  Melee)
  def simpleRanged  = WeaponProficiency(Simple,  Ranged)
  def martialMelee  = WeaponProficiency(Martial, Melee)
  def martialRanged = WeaponProficiency(Martial, Ranged)
}



case class Class (
  name: String,
  hitDie: Die,
  armorProficiencies: Set[ArmorCategory],
  weaponProficiencies: Set[WeaponProficiency],
  toolProficiencies: Set[Tool],
  rollModifiers: Map[RollType, RollModifier],
  skills: CharacterParameter[Skill]
)

// modelling examples:
// - You gain a +2 bonus to attack rolls you make with ranged weapons
// - Your weapon scores a critical hit on a roll of 19 or 20
// - Once per turn, you can deal an extra 1d6 damage to one creature you hit with an attack
//   if you have advantage on the attack roll. The attack must use a finesse or a ranged weapon.
// - More attacks per turn   => num attacks per turn on character
// - Re-roll failed saving throw once   => num allowed rerolls on roll types


case class ACBonus(
  value: Int,
  description: String
)

case class ArmorClass(
  baseAc: Int = Armor.none.ac,
  baseArmor: Armor  = Armor.none,
  bonuses: Set[ACBonus] = Set()) {

  def withBonus(bonus: ACBonus) =
    this.copy(bonuses = bonuses + bonus)
}

sealed trait ActionType
object ActionType {
  case object Move extends ActionType
  case object Normal extends ActionType
  case object Bonus extends ActionType
  case object Reaction extends ActionType
}

trait Action { val actionType: ActionType }

case class AttackAction(
  weapon: Weapon,
  attackModifiers: Set[CheckModifier] = Set(),
  damageModifiers: Set[RollModifier] = Set()) extends Action {

  val actionType = ActionType.Normal

  def withAttackModifier(mod: CheckModifier) =
    this.copy(attackModifiers = attackModifiers + mod)
}


case class Sheet(
  // wields
  primaryWeapon: Option[Weapon] = None,
  secondaryWeapon: Option[Weapon] = None,
  armor: Option[Armor] = None,

  // abilities
  actions: Set[Action] = Set(),
  ac: ArmorClass = ArmorClass()
) {
  def withAction(a: Action) = this.copy(actions = actions + a)
}


object Sheet {
  def empty = Sheet()

  type SheetTransform = Sheet => Sheet
}


object FightingStyles {
  def archery(s: Sheet) =
    s.copy(actions = s.actions.map {
      case a @ AttackAction(weapon, _, _) if weapon.combatType == Ranged =>
        a.withAttackModifier(RollBonus(2, "archery"))
      case other => other
    })


  def defense(s: Sheet) = {
    s.armor match {
      case Some(_) => s.copy(ac = s.ac.withBonus(ACBonus(1, "defense")))
    }
  }
}


object Classes {
  def fighter = Class(
    name = "fighter",
    hitDie = d10,
    armorProficiencies = Set(LightArmor, MediumArmor, HeavyArmor, ShieldArmor),
    weaponProficiencies = Set(WeaponProficiency.simpleMelee,
                              WeaponProficiency.simpleRanged,
                              WeaponProficiency.martialMelee,
                              WeaponProficiency.martialRanged),
    toolProficiencies = Set(),
    rollModifiers = Map(
      (StrSave -> Advantage),
      (DexSave -> Advantage)
    ),
    skills = CharacterParameter(2, Set(Acrobatics, AnimalHandling, Athletics, History, Insight, Intimidation, Perception, Survival))
  )
 }
