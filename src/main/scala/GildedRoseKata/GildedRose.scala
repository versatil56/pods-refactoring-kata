package GildedRoseKata

class GildedRose(val items: Array[Item]) {

  import Inventory._

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

  def updateQuality() {

   items.foreach { item =>
     if(item.name == Sulfuras){
       item.quality
     } else {
       item.name match {
         case AgedBrie =>
           (increaseQuality andThen reduceSellIn andThen increaseQualityIfSellInApproaches)(item)
           
         case "Backstage passes to a TAFKAL80ETC concert" =>
           if (item.quality < 50) {
             item.quality = item.quality + 1

               if (item.sellIn < 11) {
                 if (item.quality < 50) {
                   item.quality = item.quality + 1
                 }
               }

               if (item.sellIn < 6) {
                 if (item.quality < 50) {
                   item.quality = item.quality + 1
                 }
               }
           }
           item.sellIn = item.sellIn - 1
           if(item.sellIn < 0) {
             item.quality = item.quality - item.quality
           }
           
         case _ =>

           if (item.quality > 0) {
               item.quality = item.quality - 1
           }
             item.sellIn = item.sellIn - 1
           if(item.sellIn < 0) {
             if (item.quality > 0) {
                 item.quality = item.quality - 1
             }
           }
           
       }
     }
    }
  }

}


