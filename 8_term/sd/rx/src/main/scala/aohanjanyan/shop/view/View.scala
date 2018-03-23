package aohanjanyan.shop.view

import aohanjanyan.shop.presenter._
import io.reactivex.netty.protocol.http.server.HttpServer
import rx.lang.scala.JavaConversions

class View(port: Int, presenter: Presenter) {
  def runServer(): Unit = {
    HttpServer
      .newServer(port)
      .start((req, res) => {
        val command = req.getDecodedPath.substring(1)
        val observable = presenter.runCommand(command)
        res.writeString(JavaConversions.toJavaObservable(observable).map("" + _))
      })
      .awaitShutdown()
  }
}
