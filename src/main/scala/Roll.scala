sealed trait RollModifier {
  def description: String
}

sealed trait CheckModifier extends RollModifier

case class RollBonus(bonus: Int, description: String) extends RollModifier with CheckModifier

case object Advantage extends CheckModifier
case object RollNeutral extends CheckModifier
case object Disadvantage extends CheckModifier




sealed trait RollType
case object DamageRoll extends RollType
case object StealthRoll extends RollType
case object PoisonSave extends RollType
case object StrSave extends RollType
case object DexSave extends RollType
case object ConSave extends RollType
case object IntSave extends RollType
case object WisSave extends RollType
case object ChaSave extends RollType

sealed trait Die { def sides: Int }
case object d4 extends Die { val sides = 4 }
case object d6 extends Die { val sides = 6 }
case object d8 extends Die { val sides = 8 }
case object d10 extends Die { val sides = 10 }
case object d12 extends Die { val sides = 12 }
case object d20 extends Die { val sides = 20 }
case object d100 extends Die { val sides = 100 }

case class Roll(n: Int, die: Die, bonus: Int = 0)
