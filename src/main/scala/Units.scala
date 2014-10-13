import squants.mass.Pounds
import squants.space.Feet
import squants.time.{Days, Seconds, TimeUnit}

object Years extends TimeUnit {
  val multiplier = Days.multiplier * 365d
  val symbol = "y"
}

object Rounds extends TimeUnit {
  val multiplier = Seconds.multiplier * 6d
  val symbol = "r"
}

case class Money(cp: Int)

object CustomUnitConversions {
  implicit class GeneralUnits(n: Int) {
    def years = Years(n)
    def rounds = Rounds(n)
    def round = Rounds(n)
    def feetPerRound = Feet(n) / Rounds(1)
    def lb = Pounds(n)
  }

  implicit class GeneralUnitsDouble(n: Double) {
    def years = Years(n)
    def rounds = Rounds(n)
    def round = Rounds(n)
    def feetPerRound = Feet(n) / Rounds(1)
    def lb = Pounds(n)
  }

  implicit class MoneyConversions(n: Int) {
    def pp = Money(n * 1000)
    def gp = Money(n * 100)
    def ep = Money(n * 50)
    def sp = Money(n * 10)
    def cp = Money(n)
  }
}
