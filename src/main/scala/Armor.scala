import CustomUnitConversions._
import scala.language.postfixOps
import squants._


sealed trait ArmorCategory
case object NoArmor extends ArmorCategory
case object LightArmor extends ArmorCategory
case object MediumArmor extends ArmorCategory
case object HeavyArmor extends ArmorCategory
case object ShieldArmor extends ArmorCategory

sealed trait ArmorDexModifier
case object NoDexModifier extends ArmorDexModifier
case object IncludeDexModifier extends ArmorDexModifier
case object IncludeDexModifierMax2 extends ArmorDexModifier

case class Armor (
  name: String,
  category: ArmorCategory,
  cost: Money,
  ac: Int,
  dexModifier: ArmorDexModifier,
  weight: Mass,
  rollModifiers: Map[RollType, RollModifier] = Map(),
  requiredStats: StatBlock = StatBlock.zero) extends Item {

  def apply(s: Sheet) = {
    s.copy(ac = s.ac.copy(baseAc = ac, baseArmor = this))
    // todo: apply roll modifiers
  }
}

object Armor {
  private val stealthDisadv: Map[RollType, RollModifier] = Map( (StealthRoll -> Disadvantage) )
  private def requireStr(str: Int) = StatBlock.zero.copy(str = str)

  val none           = Armor("None",                NoArmor,     0 gp, 10, IncludeDexModifier,  0 lb)

  val padded         = Armor("Padded",           LightArmor,     5 gp, 11, IncludeDexModifier,  8 lb, rollModifiers = stealthDisadv)

  val leather        = Armor("Leather",          LightArmor,     5 gp, 11, IncludeDexModifier, 10 lb)
  val studdedLeather = Armor("Studded leather",  LightArmor,    45 gp, 12, IncludeDexModifier, 13 lb)

  val hide           = Armor("Hide",            MediumArmor,    10 gp, 12, IncludeDexModifierMax2, 12 lb)
  val chainShirt     = Armor("Chain shirt",     MediumArmor,    50 gp, 13, IncludeDexModifierMax2, 20 lb)
  val scaleMail      = Armor("Scale mail" ,     MediumArmor,    50 gp, 14, IncludeDexModifierMax2, 45 lb, rollModifiers = stealthDisadv)
  val breastplate    = Armor("Breastplate" ,    MediumArmor,   400 gp, 14, IncludeDexModifierMax2, 20 lb)
  val halfPlate      = Armor("Half plate",      MediumArmor,   750 gp, 15, IncludeDexModifierMax2, 40 lb, rollModifiers = stealthDisadv)

  val ringMail       = Armor("Ring mail",        HeavyArmor,    30 gp, 14, NoDexModifier, 40 lb, rollModifiers = stealthDisadv)
  val chainMail      = Armor("Chain mail",       HeavyArmor,    75 gp, 16, NoDexModifier, 55 lb, rollModifiers = stealthDisadv, requiredStats = requireStr(13))
  val splint         = Armor("Splint",           HeavyArmor,   200 gp, 17, NoDexModifier, 60 lb, rollModifiers = stealthDisadv, requiredStats = requireStr(15))
  val plate          = Armor("Plate",            HeavyArmor,  1500 gp, 18, NoDexModifier, 65 lb, rollModifiers = stealthDisadv, requiredStats = requireStr(15))

  // TODO: shield
}
