package aohanjanyan.shop.model

import money.Currency
import org.bson.Document

case class User(id: Int, currency: Currency) {
  def this(doc: Document) {
    this(doc.getInteger("id"), Currency(doc.getString("currency")))
  }
}