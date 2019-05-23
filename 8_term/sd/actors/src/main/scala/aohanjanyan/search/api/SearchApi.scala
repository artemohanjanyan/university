package aohanjanyan.search.api

case class SearchResult(apiName: String, links: List[String])

trait SearchApi {
  def search(request: String): SearchResult
}

abstract class AbstractSearchApi extends SearchApi {

  def getApiName: String

  protected def doSearch(request: String): List[String]

  override def search(request: String): SearchResult =
    SearchResult(getApiName, doSearch(request).take(5))
}