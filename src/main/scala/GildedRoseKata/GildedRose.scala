package GildedRoseKata

class GildedRose(val items: Array[Item]) {

  val agedBrie = "Aged Brie"
  val backstage = "Backstage passes to a TAFKAL80ETC concert"
  val sulphuras = "Sulfuras, Hand of Ragnaros"

  def updateQuality(): Unit = {
    items.foreach { it =>
      changeSellIn(it)
      it.name match {
        case `backstage` =>
          qualityPlusOne(it)

          if (it.sellIn < 10)
            qualityPlusOne(it)


          if (it.sellIn < 5)
            qualityPlusOne(it)

          if (it.sellIn < 0)
            it.quality = 0

        case `agedBrie` =>
          qualityPlusOne(it)
          if (it.sellIn < 0)
            qualityPlusOne(it)

        case `sulphuras` => ()

        case _ =>
          qualityMinusOne(it)
          if (it.sellIn < 0)
            qualityMinusOne(it)
      }
    }
  }

  private def qualityPlusOne(it: Item): Unit = {
    if (it.quality < 50) {
      it.quality = it.quality + 1
    }
  }

  private def changeSellIn(it: Item): Unit = {
    if (!it.name.equals("Sulfuras, Hand of Ragnaros")) {
      it.sellIn = it.sellIn - 1
    }
  }

  private def qualityMinusOne(it: Item): Unit = {
    if (it.quality > 0)
      it.quality = it.quality - 1
  }

}

