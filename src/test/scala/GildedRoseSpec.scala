import GildedRoseKata.{GildedRose, GoldenMaster, Item}
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks


class GildedRoseSpec extends FlatSpec with Matchers with PropertyChecks {

  import GildedRoseKata.Inventory._

  def copyArray(arr: Array[Item]): Array[Item] = arr.clone().map(_.copy())

  val sellInDays = Gen.chooseNum[Int](-10, 10)
  val qualities = Gen.posNum[Int]

  val itemNames = Gen.oneOf(
    NormalItem,
    AgedBrie,
    Sulfuras,
    BackstagePass,
    Vest
  )

  val items: Gen[Item] = for {
    name <- itemNames
    sellIn <- sellInDays
    quality <- qualities
  } yield Item(name, sellIn, quality)

  val itemArrays: Gen[Array[Item]] = Gen.containerOf[Array, Item](items)

  val daysToPass: Gen[Int] = Gen.posNum[Int]

  "The refactored gilded rose" should "behave like the golden master" in {
    forAll(daysToPass, itemArrays.map(items => (copyArray(items), items))) { case (days, (items1, items2)) =>

      val master = new GoldenMaster(items1)
      val instance = new GildedRose(items2)

      for (_ <- 1 to days) {
        master.updateGoldenMasterQuality()
        instance.updateQuality()
      }

      instance.items.length shouldBe master.items.length
      instance.items shouldBe master.items

      for((item,x)<-master.items.zipWithIndex){
        val itemToCompare = instance.items(x)
        println(s"\n ${item.name} - ${item.quality} - ${item.sellIn} ==== ${itemToCompare.name} - ${itemToCompare.quality} - ${itemToCompare.sellIn}")
      }
    }
  }
  
}
