package GildedRoseKata

class GoldenMaster(val items: Array[Item]) {
  def updateGoldenMasterQuality() {
    for (i <- items.indices) {

      if (items(i).name.equals("Aged Brie") || items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
        if (items(i).quality < 50) {
          items(i).quality = increaseQuality(i)
        }
      } else {
        if (items(i).quality > 0) {
          if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
            items(i).quality = decreaseQuality(i)
          }
        }
      }

      processConcertTickets(i)

      if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
        items(i).bestBeforeDate = reduceBestBeforeDate(i)
      }

      processItemsOutOfDate(i)
    }
  }

  private def processConcertTickets(i: Int) = {
    if (items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
      if (items(i).quality < 50) {
        if (items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
          if (items(i).bestBeforeDate < 11) {
            processItemWithLessThan50Quality(i)
          }
          if (items(i).bestBeforeDate < 6) {
            processItemWithLessThan50Quality(i)
          }
        }
      }
    }
  }

  private def processItemWithLessThan50Quality(i: Int) = {
    if (items(i).quality < 50) {
      items(i).quality = increaseQuality(i)
    }
  }

  private def processItemsOutOfDate(i: Int) = {
    if (items(i).bestBeforeDate < 0) {
      if (items(i).name.equals("Aged Brie")) {
        processItemWithLessThan50Quality(i)
      } else {
        if (items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
          items(i).quality = removeAllQuality(i)
        } else {
          if (items(i).quality > 0) {
            if (items(i).name.equals("Sulfuras, Hand of Ragnaros")) {

            } else {
              items(i).quality = decreaseQuality(i)
            }
          }
        }
      }
    }
  }

  private def reduceBestBeforeDate(i: Int) = {
    items(i).bestBeforeDate - 1
  }

  private def removeAllQuality(i: Int) = {
    items(i).quality - items(i).quality
  }

  private def decreaseQuality(i: Int) = {
    items(i).quality - 1
  }

  private def increaseQuality(i: Int) = {
    items(i).quality + 1
  }
}

