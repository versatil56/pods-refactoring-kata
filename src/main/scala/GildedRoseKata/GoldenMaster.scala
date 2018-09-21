package GildedRoseKata

class GoldenMaster(val items: Array[Item]) {
  def updateGoldenMasterQuality() {
    for (i <- items.indices) {
      if (items(i).name.equals("Aged Brie") || items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
        if (items(i).quality < 50) {
          items(i).quality = increaseQuality(i)
          if (items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            if (items(i).sellIn < 11) {
              if (items(i).quality < 50) {
                items(i).quality = increaseQuality(i)
              }
            }

            if (items(i).sellIn < 6) {
              if (items(i).quality < 50) {
                items(i).quality = increaseQuality(i)
              }
            }
          }
        }
      } else {
        if (items(i).quality > 0) {
          if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
            items(i).quality = decreaseQuality(i)
          }
        }
      }

      if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
        items(i).sellIn = reducePrice(i)
      }

      if (items(i).sellIn < 0) {
        if (items(i).name.equals("Aged Brie")) {
          if (items(i).quality < 50) {
            items(i).quality = increaseQuality(i)
          }
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
  }

  private def reducePrice(i: Int) = {
    items(i).sellIn - 1
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

