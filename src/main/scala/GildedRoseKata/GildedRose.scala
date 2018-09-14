package GildedRoseKata

class GildedRose(val items: Array[Item]) {

  import Inventory._

  def updateQuality() {
   items.foreach { item =>

      if (!item.name.equals(AgedBrie) && !item.name.equals(BackstagePass)) {
        reduceQuality(item)
      } else {
        increaseQuality(item)
      }

      if (!item.name.equals(Sulfuras)) {
        item.sellIn = item.sellIn - 1
      }

      if (item.sellIn < 0) {
        item.name match {
          case AgedBrie if item.quality < 50 => item.quality = item.quality + 1
          case AgedBrie => ()
          case BackstagePass => item.quality = 0
          case _ => reduceQuality(item)
        }
      }

    }
  }

  private def reduceQuality(item: Item): Unit = {
    if (item.quality > 0) {
      if (!item.name.equals(Sulfuras)) {
        item.quality = item.quality - 1
      }
    }
  }

  def increaseQuality(item: Item): Unit = {
    if (item.quality < 50) {
      item.name match {
        case BackstagePass if item.sellIn < 6 => item.quality = item.quality + 3
        case BackstagePass if item.sellIn < 11 & item.sellIn >= 6 => item.quality = item.quality + 2
        case _ => item.quality = item.quality + 1
      }
    }
  }

}


