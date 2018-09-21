package GildedRoseKata

class GoldenMaster(val items: Array[Item]) {
  def updateGoldenMasterQuality() {
    for (currentItem <- items.indices) {
      if (notALegendaryItem(currentItem)) {
        processAgedBrie(currentItem)

        processNormalItems(currentItem)

        processConcertTickets(currentItem)

        reduceBestBeforeDate(currentItem)

        processItemsOutOfDate(currentItem)
      }
    }
  }

  private def processNormalItems(currentItem: Int) = {
    if (notASpecialItem(currentItem)) {
      if (items(currentItem).quality > 0) {
        items(currentItem).quality = decreaseQuality(currentItem)
      }
    }
  }

  private def processAgedBrie(currentItem: Int) = {
    if (items(currentItem).name.equals("Aged Brie")) {
      if (items(currentItem).quality < 50) {
        items(currentItem).quality = increaseQuality(currentItem)
      }
    }
  }

  private def notASpecialItem(currentItem: Int) = {
    !items(currentItem).name.equals("Aged Brie") && !items(currentItem).name.equals("Backstage passes to a TAFKAL80ETC concert")
  }

  private def notALegendaryItem(i: Int) = {
    !items(i).name.equals("Sulfuras, Hand of Ragnaros")
  }

  private def processConcertTickets(i: Int): Unit = {
    if (items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
      if (items(i).quality < 50) {
        items(i).quality = increaseQuality(i)
      }
    }

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
    items(i).bestBeforeDate = items(i).bestBeforeDate - 1
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

