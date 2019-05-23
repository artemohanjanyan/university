package aohanjanyan.search.api

class GnibApi extends BingApi {

  override def getApiName: String = "Gnib"

  override def search(request: String): SearchResult = super.search(request.reverse)
}
