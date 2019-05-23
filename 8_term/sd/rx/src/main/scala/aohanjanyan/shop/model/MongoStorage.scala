package aohanjanyan.shop.model

import com.mongodb.client.model.Filters
import com.mongodb.rx.client.MongoDatabase
import money._
import org.bson.Document
import rx.lang.scala.schedulers.IOScheduler
import rx.lang.scala.{JavaConversions, Observable}

class MongoStorage(val db: MongoDatabase) extends Storage {

  override def getUserById(id: Int): Observable[User] = {
    JavaConversions
      .toScalaObservable(
        db.getCollection("user")
          .find(Filters.eq("id", id))
          .toObservable
      )
      .map(new User(_))
      .subscribeOn(IOScheduler())
  }

  override def listItems(user: User): Observable[(String, Double)] = {
    JavaConversions
      .toScalaObservable(
        db.getCollection("item")
        .find()
        .toObservable
      )
      .map(new Item(_))
      .map(_.currencyPresentation(user.currency))
      .subscribeOn(IOScheduler())
  }

  override def registerUser(id: Int, currency: Currency): Observable[Boolean] = {
    getUserById(id)
      .singleOption
      .flatMap({
        case Some(_) => Observable.just(false)
        case None =>
          JavaConversions
            .toScalaObservable(
              db.getCollection("user")
                .insertOne(new Document("id", id)
                  .append("currency", currency.getCode))
                .asObservable()
            )
            .nonEmpty
      })
  }

  override def addItem(name: String, price: (Double, Currency)): Observable[Boolean] = {
    JavaConversions
      .toScalaObservable(
        db.getCollection("item")
          .insertOne(
            new Document("name", name)
              .append("price", price._1)
              .append("currency", price._2.getCode)
          )
      )
      .nonEmpty
  }
}