package aohanjanyan.shop.presenter

import money.Currency

abstract sealed class Request
case class RegisterUserRequest(id: Int, currency: Currency) extends Request
case class ListItemsRequest(id: Int) extends Request
case class AddItemRequest(name: String, price: (Double, Currency)) extends Request