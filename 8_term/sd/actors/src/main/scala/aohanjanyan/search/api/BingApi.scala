package aohanjanyan.search.api

import java.net.{URL, URLEncoder}
import java.util.Scanner
import javax.net.ssl.HttpsURLConnection

import io.circe.Json
import io.circe.optics.JsonPath.root
import io.circe.parser.parse

class BingApi extends AbstractSearchApi {

  override def getApiName: String = "Bing"

  private val subscriptionKey = "04ae05363afc4f86b64cff8a024eea7e"
  private val host = "https://api.cognitive.microsoft.com/bing/v7.0/search"
  private val _results = root.webPages.value.each.url.string

  override def doSearch(request: String): List[String] = {
    val url = new URL(host + "?q=" + URLEncoder.encode(request, "UTF-8"))
    val connection = url.openConnection.asInstanceOf[HttpsURLConnection]
    connection.setRequestProperty("Ocp-Apim-Subscription-Key", subscriptionKey)

    val stream = connection.getInputStream
    val response = new Scanner(stream).useDelimiter("\\A").next()

    stream.close()

    val doc = parse(response).getOrElse(Json.Null)
    _results.getAll(doc)
  }
}
