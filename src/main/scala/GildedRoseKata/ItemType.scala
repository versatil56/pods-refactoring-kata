package GildedRoseKata

sealed trait ItemType {
  def item: Item
  def updateQuality(): ItemType
}

object ItemType {

  val maximumQuality: Int = 50
  val minimumQuality: Int = 0

  def sellByDatePassed(sellIn: Int): Boolean = sellIn < 0

  def canIncreaseQuality(quality: Int): Boolean = quality < maximumQuality

  def canDecreaseQuality(quality: Int): Boolean = quality > minimumQuality

  def incrementQuality(quality: Int, increment: Int): Int = {
    Math.min(quality + increment, maximumQuality)
  }

  def decrementQuality(quality: Int, decrement: Int): Int = {
    Math.max(quality - decrement, minimumQuality)
  }

  def forItem(item: Item): ItemType = {

    item.name match {
      case "Aged Brie" => AgedBrie(item)
      case "Backstage passes to a TAFKAL80ETC concert" =>  BackstagePass(item)
      case "Sulfuras, Hand of Ragnaros" => Sulfuras(item)
      case _ => StandardItem(item)
    }

  }

}

case class AgedBrie(item: Item) extends ItemType {

  import ItemType._

  override def updateQuality(): ItemType = {

    val sellIn = item.sellIn - 1

    val quality =
      if (sellByDatePassed(sellIn) && canIncreaseQuality(item.quality)) {
        incrementQuality(item.quality, 2)
      } else if (canIncreaseQuality(item.quality)) {
        incrementQuality(item.quality, 1)
      } else {
        item.quality
      }

    AgedBrie(item.copy(sellIn = sellIn, quality = quality))

  }

}

case class BackstagePass(item: Item) extends ItemType {

  import ItemType._

  override def updateQuality(): ItemType = {

    val sellIn = item.sellIn - 1

    val quality =
      if (sellByDatePassed(sellIn)) {
        0
      } else if (canIncreaseQuality(item.quality)) {
        Math.min(
          if (sellIn < 5) {
            item.quality + 3
          } else if (sellIn < 10) {
            item.quality + 2
          } else {
            item.quality + 1
          },
          maximumQuality
        )
      } else {
        item.quality
      }

    BackstagePass(item.copy(sellIn = sellIn, quality = quality))

  }

}

case class Sulfuras(item: Item) extends ItemType {
  override def updateQuality(): ItemType = this
}

case class StandardItem(item: Item) extends ItemType {

  import ItemType._

  override def updateQuality(): ItemType = {

    val sellIn = item.sellIn - 1

    val quality =
      if (sellByDatePassed(sellIn) && canDecreaseQuality(item.quality)) {
        decrementQuality(item.quality, 2)
      } else if (canDecreaseQuality(item.quality)) {
        decrementQuality(item.quality, 1)
      } else {
        item.quality
      }

    StandardItem(item.copy(sellIn = sellIn, quality = quality))

  }

}
