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

  private def processConcertTickets(currentItem: Int): Unit = {
    if (items(currentItem).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
      if (qualityLessThanFifty(currentItem)) {
        increaseQuality(currentItem)

        if (items(currentItem).bestBeforeDate < 11) {
          processItemWithLessThan50Quality(currentItem)
        }
        if (items(currentItem).bestBeforeDate < 6) {
          processItemWithLessThan50Quality(currentItem)
        }
      }
    }
  }

  private def processItemWithLessThan50Quality(currentItem: Int) = {
    if (qualityLessThanFifty(currentItem)) {
      increaseQuality(currentItem)
    }
  }

  private def processItemsOutOfDate(currentItem: Int) = {
    if (items(currentItem).bestBeforeDate < 0) {
      if (items(currentItem).name.equals("Aged Brie")) {
        processItemWithLessThan50Quality(currentItem)
      } else {
        if (items(currentItem).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
          items(currentItem).quality = removeAllQuality(currentItem)
        } else {
          if (qualityIsOver0(currentItem)) {
              items(currentItem).quality = decreaseQuality(currentItem)
          }
        }
      }
    }
  }

  private def qualityLessThanFifty(currentItem: Int) = {
    items(currentItem).quality < 50
  }

  private def reduceBestBeforeDate(currentItem: Int) = {
    items(currentItem).bestBeforeDate = items(currentItem).bestBeforeDate - 1
  }

  private def removeAllQuality(currentItem: Int) = {
    items(currentItem).quality - items(currentItem).quality
  }

  private def decreaseQuality(currentItem: Int) = {
    items(currentItem).quality - 1
  }

  private def increaseQuality(currentItem: Int) = {
    items(currentItem).quality =  items(currentItem).quality + 1
  }

  private def processNormalItems(currentItem: Int) = {
    if (notASpecialItem(currentItem) && qualityIsOver0(currentItem)) items(currentItem).quality = decreaseQuality(currentItem)
  }

  private def qualityIsOver0(currentItem: Int) = {
    items(currentItem).quality > 0
  }

  private def processAgedBrie(currentItem: Int) = {
    if (items(currentItem).name.equals("Aged Brie") && qualityLessThanFifty(currentItem)) increaseQuality(currentItem)
  }

  private def notASpecialItem(currentItem: Int) = {
    !items(currentItem).name.equals("Aged Brie") && !items(currentItem).name.equals("Backstage passes to a TAFKAL80ETC concert")
  }

  private def notALegendaryItem(i: Int) = {
    !items(i).name.equals("Sulfuras, Hand of Ragnaros")
  }
}

