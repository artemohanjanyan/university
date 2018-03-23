package aohanjanyan.shop.presenter

import aohanjanyan.shop.model.Storage
import money.Currency
import rx.lang.scala.Observable

import scala.util.Try

class Presenter(storage: Storage) {
  private def decodeRequest(request: String): Option[Request] = request.split("/") match {
    case Array("register", id, currency) =>
      for {
        id <- Try(id.toInt).toOption
        currency <- Try(Currency(currency)).toOption
      } yield RegisterUserRequest(id, currency)
    case Array("list", id) =>
      for {
        id <- Try(id.toInt).toOption
      } yield ListItemsRequest(id)
    case Array("add", name, price, currency) =>
      for {
        price <- Try(price.toDouble).toOption
        currency <- Try(Currency(currency)).toOption
      } yield AddItemRequest(name, (price, currency))
    case _ => None
  }

  private def runRequest(request: Request): Observable[String] = request match {
    case RegisterUserRequest(id, currency) =>
      storage.registerUser(id, currency)
        .map(if (_) "done" else "error")
    case ListItemsRequest(id) =>
      storage.getUserById(id)
        .flatMap(storage.listItems)
        .map(item => f"${item._1}%s: ${item._2}%.2f")
        .toList
        .map(l => l.mkString("\n"))
        .onErrorReturn(_ => "error")
    case AddItemRequest(name, price) =>
      storage.addItem(name, price)
        .map(if (_) "done" else "error")
  }

  def runCommand(command: String): Observable[String] = {
    Observable.just(decodeRequest(command))
      .flatMap(_ match {
        case None => Observable.just("bad command")
        case Some(request) => runRequest(request)
      })
  }
}
