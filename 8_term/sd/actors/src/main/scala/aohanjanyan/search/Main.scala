package aohanjanyan.search

import akka.actor.{Actor, Props}
import aohanjanyan.search.aggregator.Aggregator
import aohanjanyan.search.api.{BingApi, GnibApi}

import scala.concurrent.duration._

object Main {

  private var requestStr: String = ""

  class MainActor extends Actor {
    override def preStart(): Unit = {
      val aggregator = context.actorOf(Props[Aggregator])
      val request = Aggregator.AggregatorRequest(List(
        new BingApi(),
        new GnibApi()
      ), requestStr, 3 seconds)
      aggregator ! request
    }

    override def receive: Receive = {
      case Aggregator.AggregatorResponse(results) =>
        println(results)
        context.stop(self)
    }
  }

  def main(args: Array[String]): Unit = {
    println("Type request...")
    requestStr = scala.io.StdIn.readLine()
    akka.Main.main(Array(classOf[MainActor].getName))
  }
}
