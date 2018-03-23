package aohanjanyan.shop.model

import money._
import rx.lang.scala.Observable
import scala.collection.mutable

class HashMapStorage extends Storage {

  private val users = mutable.HashMap[Int, User]()
  private val items = mutable.ArrayBuffer[Item]()

  override def getUserById(id: Int): Observable[User] = Observable.from(users.get(id))

  override def listItems(user: User): Observable[(String, Double)] =
    Observable.from(items.map(_.currencyPresentation(user.currency)))

  override def registerUser(id: Int, currency: Currency): Observable[Boolean] = {
    Observable.just(
      if (users.contains(id)) {
        false
      } else {
        users.put(id, User(id, currency))
        true
      }
    )
  }

  override def addItem(name: String, price: (Double, Currency)): Observable[Boolean] = {
    items.append(Item(name, price))
    Observable.just(true)
  }
}
