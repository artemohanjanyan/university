package aohanjanyan.shop.model

import money._
import org.bson.Document

case class Item(name: String, price: (Double, Currency)) {

  implicit private val conversion: Conversion = Map(
    (EUR, USD) -> 1.22552,
    (EUR, RUB) -> 70.5538284,
    (USD, EUR) -> 0.815980155,
    (USD, RUB) -> 57.3394495,
    (RUB, EUR) -> 0.0141735753,
    (RUB, USD) -> 0.01737
  )

  implicit private val converter: Converter = Converter(conversion)

  def this(doc: Document) {
    this(
      doc.getString("name"),
      (doc.getDouble("price"), Currency(doc.getString("currency")))
    )
  }

  def currencyPresentation(newCurrency: Currency): (String, Double) = {
    (name, (price._1(price._2) to newCurrency).amount.doubleValue())
  }
}