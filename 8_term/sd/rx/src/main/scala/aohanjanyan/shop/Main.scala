package aohanjanyan.shop

import aohanjanyan.shop.model.MongoStorage
import aohanjanyan.shop.presenter.Presenter
import aohanjanyan.shop.view.View
import com.mongodb.rx.client.MongoClients

object Main {
  def main(args: Array[String]): Unit = {
    new View(
      8080,
      new Presenter(
        new MongoStorage(
          MongoClients.create("mongodb://localhost:27017")
            .getDatabase("shop")
        )
      )
    ).runServer()
  }
}