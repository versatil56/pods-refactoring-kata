package example

import gildedRose.{GildedRose, Item}
import goldenMaster.GoldenMaster
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks


class GildedRoseSpec extends FlatSpec with Matchers with PropertyChecks {

  def copyArray(arr: Array[Item]): Array[Item] = arr.clone().map(_.copy())

  val sellInDays = Gen.chooseNum[Int](-10, 10)
  val qualities = Gen.posNum[Int]
  val itemNames = Gen.oneOf(
    "Normal item",
    "Aged Brie",
    "Sulfuras, Hand of Ragnaros",
    "Backstage passes to a TAFKAL80ETC concert",
    "+5 Dexterity Vest"
  )

  val items: Gen[Item] = for {
    name <- itemNames
    sellIn <- sellInDays
    quality <- qualities
  } yield new Item(name, sellIn, quality)

  val itemArrays = Gen.containerOf[Array, Item](items)

  val daysToPass: Gen[Int] = Gen.posNum[Int]

  "The refactored gilded rose" should "behave like the golden master" in {
    forAll(daysToPass, itemArrays.map(a => (copyArray(a), a))) { case (days, (items1, items2)) =>

      val master = new GoldenMaster(items1)
      val instance = new GildedRose(items2)

      for (_ <- 1 to days) {
        master.updateGoldenMasterQuality()
        instance.updateQuality()
      }

      instance.items shouldBe master.items
    }
  }
}
