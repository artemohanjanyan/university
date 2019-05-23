package aohanjanyan.shop.model

import money.Currency
import rx.lang.scala.Observable

trait Storage {
  def getUserById(id: Int): Observable[User]
  def listItems(user: User): Observable[(String, Double)]

  def registerUser(id: Int, currency: Currency): Observable[Boolean]

  def addItem(name: String, price: (Double, Currency)): Observable[Boolean]
}