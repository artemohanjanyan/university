package aohanjanyan.search.aggregator

import akka.actor.Actor
import aohanjanyan.search.api.{SearchApi, SearchResult}

object Searcher {
  case class SearchRequest(request: String, api: SearchApi)
  case class SearchResponse(result: SearchResult)
}

class Searcher extends Actor {
  override def receive: Receive = {
    case Searcher.SearchRequest(request, api) =>
      val response = api.search(request)
      context.parent ! Searcher.SearchResponse(response)
      context.stop(self)
  }
}
