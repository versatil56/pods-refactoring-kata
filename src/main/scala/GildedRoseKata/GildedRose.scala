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
           if (item.quality < 50) {
             item.quality = item.quality + 1

             if (item.name.equals(BackstagePass)) {
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
           }
           item.sellIn = item.sellIn - 1

           if(item.sellIn < 0) {
             if (item.quality < 50) {
               item.quality = item.quality + 1
             }
           }
           
         case BackstagePass =>
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


