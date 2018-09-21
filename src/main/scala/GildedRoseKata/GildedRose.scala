package GildedRoseKata

class GildedRose(val items: Array[Item]) {

  import Inventory._

  def updateQuality() {

   items.foreach { item =>
     if(item.name == Sulfuras){
       item.quality
     } else {
       item.name match {
         case AgedBrie =>
           (increaseQuality andThen reduceSellIn andThen increaseQualityIfSellInApproaches)(item)

         case "Backstage passes to a TAFKAL80ETC concert" =>
           (increaseQuality andThen increaseQualityWhen10DaysOrLess andThen increaseQualityWhen5DaysOrLess andThen reduceSellIn andThen zeroQuality)(item)

         case _ =>
           (reduceQuality andThen reduceSellIn andThen reduceQualityBasedOnSellIn)(item)
       }
     }
    }
  }

  val reduceQuality = (item: Item) => {
    if (item.quality > 0) {
      item.quality = item.quality - 1
      item
    } else item
  }

  val reduceQualityBasedOnSellIn = (item: Item) => {
    if(item.sellIn < 0) {
      if (item.quality > 0) {
        item.quality = item.quality - 1
      }
      item
    } else item
  }


  val increaseQuality= (item: Item) =>
  {
    if (item.quality < 50) {
      item.quality = item.quality + 1
      item
    } else item
  }

  val increaseQualityIfSellInApproaches= (item: Item) =>
    if(item.sellIn < 0) {
      if (item.quality < 50) {
        item.quality = item.quality + 1
        item
      }
    } else item

  val reduceSellIn = (item: Item) => {
    item.sellIn = item.sellIn - 1
    item
  }

  val increaseQualityWhen10DaysOrLess = (item: Item) => {
    if (item.sellIn < 11) {
      if (item.quality < 50) {
        item.quality = item.quality + 1
      }
      item
    } else item
  }

  val increaseQualityWhen5DaysOrLess = (item: Item) => {
    if (item.sellIn < 6) {
      if (item.quality < 50) {
        item.quality = item.quality + 1
      }
      item
    } else item
  }

  val zeroQuality = (item: Item) => {
    if(item.sellIn < 0) {
      item.quality = 0
      item
    } else item
  }
}


