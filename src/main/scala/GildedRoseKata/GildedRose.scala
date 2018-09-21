package GildedRoseKata

class GildedRose(val items: Array[Item]) {

  def updateQuality(): Unit = {

    for (i <- items.indices) {
      items(i) = ItemType.forItem(items(i)).updateQuality().item
    }

  }

}
