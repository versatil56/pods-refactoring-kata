package GildedRoseKata

class GildedRose(val items: Array[Item]) {

  def updateQuality(): Unit = {

    for (i <- items.indices) {
      items(i).name match {
        case "Aged Brie" => items(i) = updateAgedBrie(items(i))
        case "Backstage passes to a TAFKAL80ETC concert" => items(i) = updateBackstagePass(items(i))
        case "Sulfuras, Hand of Ragnaros" => items(i) = updateSulfuras(items(i))
        case _ => items(i) = updateStandardItem(items(i))
      }
    }

  }

  private def updateAgedBrie(item: Item): Item = {

    val quality =
      if (canIncreaseQuality(item.quality)) {
        item.quality + 1
      } else {
        item.quality
      }

    val sellIn = item.sellIn - 1

    val quality2 =
      if (sellByDatePassed(sellIn) && canIncreaseQuality(quality)) {
        quality + 1
      } else {
        quality
      }

    item.copy(sellIn = sellIn, quality = quality2)

  }

  private def updateBackstagePass(item: Item): Item = {

    val quality =
      if (canIncreaseQuality(item.quality)) {
        Math.min(
          if (item.sellIn <= 5) {
            item.quality + 3
          } else if (item.sellIn <= 10) {
            item.quality + 2
          } else {
            item.quality + 1
          },
          50
        )
      } else {
        item.quality
      }

    val sellIn = item.sellIn - 1

    val quality2 =
      if (sellByDatePassed(sellIn)) {
        0
      } else {
        quality
      }

    item.copy(sellIn = sellIn, quality = quality2)

  }

  private def updateSulfuras(item: Item): Item = {

    item

  }

  private def updateStandardItem(item: Item): Item = {

    val quality =
      if (canDecreaseQuality(item.quality)) {
        item.quality - 1
      } else {
        item.quality
      }

    val sellIn = item.sellIn - 1

    val quality2 =
      if (sellByDatePassed(sellIn) && canDecreaseQuality(quality)) {
        quality - 1
      } else {
        quality
      }

    item.copy(sellIn = sellIn, quality = quality2)

  }

  private def sellByDatePassed(sellIn: Int): Boolean = sellIn < 0

  private def canIncreaseQuality(quality: Int): Boolean = quality < 50

  private def canDecreaseQuality(quality: Int): Boolean = quality > 0

}
