package aohanjanyan.search.aggregator

import akka.actor.{Actor, Props, ReceiveTimeout}
import aohanjanyan.search.api.{SearchApi, SearchResult}

import scala.concurrent.duration._

object Aggregator {

  case class AggregatorRequest(apis: List[SearchApi], request: String, timeout: Duration)

  case class AggregatorResponse(results: List[SearchResult])

}

class Aggregator extends Actor {

  override def receive: Receive = {
    case Aggregator.AggregatorRequest(apis, request, timeout) =>
      for (api <- apis) {
        val searcher = context.actorOf(Props[Searcher])
        searcher ! Searcher.SearchRequest(request, api)
      }
      context.setReceiveTimeout(timeout)
      context become waitApis(List(), apis.size)
  }

  def waitApis(results: List[SearchResult], left: Int): Receive = {
    case Searcher.SearchResponse(searchResult) =>
      val newResults = searchResult :: results
      if (left == 1) {
        reportResults(newResults)
      } else {
        context become waitApis(newResults, left - 1)
      }
    case ReceiveTimeout => reportResults(results)
  }

  def reportResults(results: List[SearchResult]): Unit = {
    println(context.parent)
    context.parent ! Aggregator.AggregatorResponse(results.reverse)
    context.stop(self)
  }
}
