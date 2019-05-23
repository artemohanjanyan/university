package aohanjanyan.shop.model

import com.mongodb.rx.client.MongoClients
import money._
import org.scalacheck.Gen._
import org.scalacheck._
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import rx.lang.scala.Observable

class MongoStorageTest
  extends FlatSpec
    with GeneratorDrivenPropertyChecks {

  type Command[T] = Storage => Observable[T]

  val userIdGen: Gen[Int] = choose(0, 30)
  val currencyGen: Gen[Currency] = Gen.oneOf(USD, EUR, RUB)

  val listItemsGen: Gen[Command[List[(String, Double)]]] =
    for {
      id <- userIdGen
    } yield { storage: Storage =>
      storage
        .getUserById(id)
        .flatMap(storage.listItems)
        .toList
    }

  val registerUserGen: Gen[Command[Boolean]] =
    for {
      id <- userIdGen
      currency <- currencyGen
    } yield { storage: Storage =>
      storage.registerUser(id, currency)
    }

  val addItemGen: Gen[Command[Boolean]] =
    for {
      name <- Gen.alphaStr
      price <- Gen.choose(0.01, 1e6)
      currency <- currencyGen
    } yield { storage: Storage =>
      storage.addItem(name, (price, currency))
    }

  def commandGen: Gen[Command[Any]] = oneOf(listItemsGen, registerUserGen, addItemGen)

  "MongoStorage" should "the same as HashMapStorage" in {
    forAll(Gen.listOf(commandGen)) { commands: List[Command[Any]] =>
      val hashMapStorage = new HashMapStorage()

      val db = MongoClients
        .create("mongodb://localhost:27017")
        .getDatabase("testShop")
      db.drop()
      val mongoStorage = new MongoStorage(db)

      commands forall { command =>
        val res1 = command(hashMapStorage).toBlocking.single
        val res2 = command(mongoStorage).toBlocking.single
        res1 == res2
      }
    }
  }

}