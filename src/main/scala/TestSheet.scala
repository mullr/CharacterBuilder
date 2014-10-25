object TestSheets {
  def pregenFighter = Sheet(
    name = "Pregen Fighter",
    levels = Map(Classes.fighter -> 1),
    race = Races.human,
    alignment = Alignment(Lawful, OrderNeutral),

    stats = StatBlock(str=15, dex=7, con=14, int=10, wis=12, cha=13),

    background = "Noble",
    personalityTraits = "",
    ideals = "",
    bonds = "",
    flaws = ""
  )
}
