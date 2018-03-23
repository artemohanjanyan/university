package aohanjanyan.shop.presenter

import aohanjanyan.shop.model.{Storage, User}
import money._
import org.scalatest.{FlatSpec, Matchers}
import rx.lang.scala.Observable

class PresenterTest extends FlatSpec with Matchers {

  abstract class AbstractStorage extends Storage {
    override def getUserById(id: Int): Observable[User] = throw new NotImplementedError
    override def listItems(user: User): Observable[(String, Double)] = throw new NotImplementedError
    override def registerUser(id: Int, currency: Currency): Observable[Boolean] = throw new NotImplementedError
    override def addItem(name: String, price: (Double, Currency)): Observable[Boolean] = throw new NotImplementedError
  }

  "Presenter" should "register user" in {
    var called = false
    new Presenter(new AbstractStorage {
      override def registerUser(id: Int, currency: Currency): Observable[Boolean] = {
        called = true
        id shouldEqual 1
        currency shouldEqual USD
        Observable.just(true)
      }
    }).runCommand("register/1/USD").toBlocking.single
    assert(called)
  }

  "Presenter" should "add item" in {
    var called = false
    new Presenter(new AbstractStorage {
      override def addItem(name: String, price: (Double, Currency)): Observable[Boolean] = {
        called = true
        name shouldEqual "iPhone"
        price shouldEqual (1, EUR)
        Observable.just(true)
      }
    }).runCommand("add/iPhone/1/EUR").toBlocking.single
    assert(called)
  }

  "Presenter" should "list items" in {
    val user = User(1, RUB)
    var called1 = false
    var called2 = false
    new Presenter(new AbstractStorage {
      override def getUserById(id: Int): Observable[User] = {
        called1 = true
        id shouldEqual 1
        Observable.just(user)
      }

      override def listItems(user1: User): Observable[(String, Double)] = {
        called2 = true
        user shouldEqual user1
        Observable.empty
      }
    }).runCommand("list/1").toBlocking.single
    assert(called1)
    assert(called2)
  }

}