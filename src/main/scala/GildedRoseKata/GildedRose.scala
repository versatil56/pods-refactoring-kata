package GildedRoseKata

class GildedRose(val items: Array[Item]) {

  private def changeQuality(i: Item, increment: Int): Unit = {
  val newQuality = if ((i.quality + increment) >= 0 && (i.quality + increment)  <= 50) {
      i.quality + increment
    } else {
      i.quality
    }
    i.quality = newQuality
  }

  private def changeSellIn(it: Item): Unit = {
    if (!it.name.equals("Sulfuras, Hand of Ragnaros")) {
      it.sellIn = it.sellIn - 1
    }
  }

  def updateQuality() {
    val agedBrie = "Aged Brie"
    val backstage = "Backstage passes to a TAFKAL80ETC concert"
    val sulphuras = "Sulfuras, Hand of Ragnaros"
    val conj = "Conjured"
    items.foreach { it =>
      (it.name, it.sellIn) match {
        case (`sulphuras`, _) => it.quality
        case (`backstage`, s) if s < 0 => 0
        case (`backstage`, s) if s <= 5 => changeQuality(it, 3)
        case (`backstage`, s) if s > 5 && s <=10 => changeQuality(it, 2)
        case (`backstage`, s) if s > 10 => changeQuality(it, 1)
        case (`agedBrie`, _) => changeQuality(it, 1)
        case (`conj`, s) if s > 0 => changeQuality(it, -2)
        case (`conj`, s) => changeQuality(it, -4)
        case (_, s) if s > 0 => changeQuality(it, -1)
        case (_, s) if s <= 0 => changeQuality(it, -2)
      }

      changeSellIn(it)
      println( "Item:" + it.toString)
    }
    //
    //   for (i <- 0 until items.length) {
    //      if (!items(i).name.equals("Aged Brie")
    //        && !items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
    //        if (items(i).quality > 0) {
    //          if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
    //            items(i).quality = items(i).quality - 1
    //          }
    //        }
    //      } else {
    //        if (items(i).quality < 50) {
    //          items(i).quality = items(i).quality + 1
    //
    //          if (items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
    //            if (items(i).sellIn < 11) {
    //              if (items(i).quality < 50) {
    //                items(i).quality = items(i).quality + 1
    //              }
    //            }
    //
    //            if (items(i).sellIn < 6) {
    //              if (items(i).quality < 50) {
    //                items(i).quality = items(i).quality + 1
    //              }
    //            }
    //          }
    //        }
    //      }
    //
    //      if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
    //        items(i).sellIn = items(i).sellIn - 1
    //      }
    //
    //      if (items(i).sellIn < 0) {
    //        if (!items(i).name.equals("Aged Brie")) {
    //          if (!items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
    //            if (items(i).quality > 0) {
    //              if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
    //                items(i).quality = items(i).quality - 1
    //              }
    //            }
    //          } else {
    //            items(i).quality = items(i).quality - items(i).quality
    //          }
    //        } else {
    //          if (items(i).quality < 50) {
    //            items(i).quality = items(i).quality + 1
    //          }
    //        }
    //      }
    //    }
  }
}

