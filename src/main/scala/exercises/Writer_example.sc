import cats._
import cats.data._
import cats.implicits._

object Tracked {
  type Tracked[A] = Writer[List[String], A]

  implicit def trackedShow[A: Show]: Show[Tracked[A]] = Show.show { ta =>
    val (log: List[String], a: A) = ta.run
    (log ++ List(a.show)).mkString("\n")
  }
}
import Tracked._

case class Client(id: Long, name: String, age: Int)
object Client {
  def makeRaw(id: Long, name: String, age: Int): Tracked[Client] = ???
}

case class Product(id: Long, name: String, unitPrice: Double)
object Product {
  def makeRaw(id: Long, name: String, unitPrice: Double): Tracked[Product] = ???
}

case class ShoppingCartItem(quantity: Int, product: Product) {
  def total: Double = quantity * product.unitPrice
}

object ShoppingCartItem {
  implicit val shoppingCartItemShow: Show[ShoppingCartItem] =
    Show.show(item => s"${item.quantity} x ${item.product.name}")

  def makeRaw(quantity: Int, productId: Long, productName: String, productUnitPrice: Double): Tracked[ShoppingCartItem] =
    ???
}
case class ShoppingCart(client: Client, items: List[ShoppingCartItem]) {
  def total: Double = items.map(_.total).sum
}

object ShoppingCart {
  implicit val scShow: Show[ShoppingCart] = Show.fromToString

  def makeRaw(
     clientId: Long,
     clientName: String,
     clientAge: Int,
     items: List[(Int, Long, String, Double)]): Tracked[ShoppingCart] =
    ???
}


sealed trait Discount {
  val name: String
  def applies(client: Client, shoppingCartItem: ShoppingCartItem): Boolean
  def getDiscountedAmount(shoppingCartItem: ShoppingCartItem): Double

  def calculateDiscount(
    client: Client,
    shoppingCartItem:
    ShoppingCartItem
  ): Tracked[Double] =
    if(applies(client, shoppingCartItem)) {
      getDiscountedAmount(shoppingCartItem)
        .writer(List(s"Applied discount: $name"))
    } else {
      0d.pure[Tracked]
    }
}

object Discount {
  object MoreThanFiveUnitsDiscount extends Discount {
    override val name = "10% discount on 5 units or more"

    override def applies(client: Client, shoppingCartItem: ShoppingCartItem): Boolean =
      shoppingCartItem.quantity > 5

    override def getDiscountedAmount(shoppingCartItem: ShoppingCartItem): Double =
      shoppingCartItem.total * 0.1
  }

  object ElderlyDiscount extends Discount {
    override val name = "20% discount for people 65 or older"

    override def applies(client: Client, shoppingCartItem: ShoppingCartItem): Boolean =
      client.age > 65

    override def getDiscountedAmount(shoppingCartItem: ShoppingCartItem): Double =
      shoppingCartItem.total * 0.2
  }

  val allDiscounts: List[Discount] = List(MoreThanFiveUnitsDiscount, ElderlyDiscount)
}

def calculateTotalDiscount(shoppingCart: ShoppingCart, discounts: List[Discount]): Tracked[Double] = {
//  (shoppingCart.items, discounts).mapN { (item, discount) =>
//    discount.calculateDiscount(shoppingCart.client, item)
//  }.combineAll
  (shoppingCart.items, discounts)
    .tupled
    .traverse { case (i, d) => d.calculateDiscount(shoppingCart.client, i) }
    .map(_.sum)
}

def calculateTotal(shoppingCart: ShoppingCart): Tracked[Double] = {
  calculateTotalDiscount(shoppingCart, Discount.allDiscounts)
    .map(a => shoppingCart.total - a)
}

val client = Client(1, "leandro", 70)
val milk = Product(1, "milk", 15.0)
val eggs = Product(1, "eggs", 25.0)
val items = List(
  ShoppingCartItem(15, milk),
  ShoppingCartItem(30, eggs)
)
val shoppingCart = ShoppingCart(client, items)
Show[Tracked[Double]].show(calculateTotalDiscount(shoppingCart, Discount.allDiscounts))
Show[Tracked[Double]].show(calculateTotal(shoppingCart))
