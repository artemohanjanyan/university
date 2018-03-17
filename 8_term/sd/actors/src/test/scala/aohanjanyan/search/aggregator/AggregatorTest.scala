package aohanjanyan.search.aggregator

import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import aohanjanyan.search.api.{SearchApi, SearchResult}

import scala.concurrent.duration._

class AggregatorTest(_system: ActorSystem)
  extends TestKit(_system)
    with Matchers
    with FlatSpecLike
    with BeforeAndAfterAll {

  def this() = this(ActorSystem("AggregatorTest"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  class DumbApi(name: String) extends SearchApi {
    override def search(request: String): SearchResult =
      SearchResult(name, List(name + " " + request))
  }

  class SlowApi() extends SearchApi {
    override def search(request: String): SearchResult = {
      while (true) {}
      null
    }
  }

  "A Searcher Actor" should "use API" in {
    val testProbe = TestProbe()
    val searcher = TestActorRef(Props[Searcher], testProbe.ref)
    val api = new DumbApi("name")
    val request = "test"
    searcher ! Searcher.SearchRequest(request, api)
    testProbe.expectMsg(500 millis, Searcher.SearchResponse(api.search(request)))
  }

  "An Aggregator Actor" should "aggregate" in {
    val testProbe = TestProbe()
    val aggregator = TestActorRef(Props[Aggregator], testProbe.ref)
    val api1 = new DumbApi("name1")
    val api2 = new DumbApi("name2")
    val request = "test"
    aggregator ! Aggregator.AggregatorRequest(List(api1, api2), request, 1 second)
    val response = List(api1.search(request), api2.search(request))
    testProbe.expectMsgAnyOf(
      500 millis,
      Aggregator.AggregatorResponse(response),
      Aggregator.AggregatorResponse(response.reverse)
    )
  }

  "An Aggregator Actor" should "not wait to long" in {
    val testProbe = TestProbe()
    val aggregator = TestActorRef(Props[Aggregator], testProbe.ref)
    val api1 = new DumbApi("name1")
    val api2 = new SlowApi()
    val request = "test"
    aggregator ! Aggregator.AggregatorRequest(List(api1, api2), request, 1 second)
    val response = Aggregator.AggregatorResponse(List(api1.search(request)))
    testProbe.expectMsg(1500 millis, response)
  }

}
