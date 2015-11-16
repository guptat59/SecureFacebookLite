import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import akka.actor._
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import spray.can.Http
import spray.http._
import spray.http.HttpMethods._
import spray.routing.HttpService
import spray.routing.SimpleRoutingApp
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.impl.Future
import scala.concurrent.ExecutionContext.Implicits.global
import spray.json._


object FacebookServer extends App with SimpleRoutingApp {

  val config: Config = ConfigFactory.parseString("""
    spray.can {
      server {        
        pipelining-limit = 10
      }      
      
      host-connector {
        pipelining = on        
      }
    }
    """)

  /**
   * client {
   * pipelining = on
   * pipelining-limit = 10
   * }
   */

  implicit val system = ActorSystem("TarunServer", config)

  startServer(interface = "localhost", port = 9443) {
    get {
      path("ping") {
        complete {
          for (i <- 1 to 5) {
            Thread.sleep(1000)
            println("pinging")
          }
          "pinged"
        }
      } ~
        get {
          path("pong") {
            complete {
              for (i <- 1 to 5) {
                Thread.sleep(1000)
                println("ponging")
              }
              "ponged"
            }
          }
        }
    }
  }

  var ref: ActorRef = system.actorOf(Props[dispatcher])
  //IO(Http) ! Http.Bind(ref, interface = "localhost", port = 9443)

  Thread.sleep(10000)
  //IO(Http).ask((HttpRequest(GET, Uri(s"http://localhost:9443/ping"))))

  def client(): Unit = {
    implicit val timeout = Timeout(10000)
    println("#####################################")
    //val futu = IO(Http) ? HttpRequest(GET, Uri(s"http://localhost:9443/ping"))
    var futu: Future[String] = ask(IO(Http), (HttpRequest(GET, Uri(s"http://localhost:9443/ping")))).mapTo[String]
    var futu2: Future[String] = ask(IO(Http), (HttpRequest(GET, Uri(s"http://localhost:9443/")))).mapTo[String]
    println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    var f = Future {
      Thread.sleep(10)
    }
    f.onComplete { x =>
      println("asking2")
      var res2 = Await.result(futu2, timeout.duration);
      println("res2 is " + res2)
    }
    var f2 = Future {
      Thread.sleep(10)
    }
    f2.onComplete { x =>
      println("asking1")
      val res = Await.result(futu, timeout.duration);
      println("res is " + res)
    }
  }
}

class dispatcher extends Actor {
  var httpListener: ActorRef = _

  trait demo extends HttpService {
    val r = "asd"
  }

  def receive = {

    case y: Http.Bound => {
      println("Server bounded at " + y.localAddress)
      httpListener = sender()
    }

    case Http.Unbound => {
      println("Server unbounded ")
    }

    case _: Http.Connected => {
      println("Client connected")
      sender ! Http.Register(context.system.actorOf(Props[act]))
    }

    case "Stop" => {
      println("Received cmd to stop")
      httpListener ! Http.Unbind
    }

    case HttpRequest(GET, Uri.Path("/"), _, _, _) => {
      println("Ping Request received at server1")
      for (i <- 1 to 5) {
        Thread.sleep(1000)
        println("default processing")
      }
      sender ! HttpResponse(entity = "PONG1")
    }

    case HttpRequest(HttpMethods.GET, Uri.Path("/ping"), _, _, _) => {
      println("Ping Request received at server2")
      for (i <- 1 to 5) {
        Thread.sleep(1000)
        println("ping processing")
      }

      sender ! HttpResponse(entity = "PiNG2")
    }

    case _ => {
      println("unknown message received" + context.sender().path);
    }
  }
}

class act extends Actor {

  var httpListener: ActorRef = _

  trait demo extends HttpService {
    val r = "asd"
  }

  def receive = {

    case y: Http.Bound => {
      println("Server bounded at " + y.localAddress)
      httpListener = sender()
    }

    case Http.Unbound => {
      println("Server unbounded ")
    }

    case _: Http.Connected => {
      println("Client connected")
      sender ! Http.Register(self)
    }

    case "Stop" => {
      println("Received cmd to stop")
      httpListener ! Http.Unbind
    }

    case HttpRequest(GET, Uri.Path("/"), _, _, _) => {
      println("Ping Request received at server1")
      for (i <- 1 to 5) {
        Thread.sleep(1000)
        println("default processing")
      }
      sender ! HttpResponse(entity = "PONG1")
    }

    case HttpRequest(HttpMethods.GET, Uri.Path("/ping"), _, _, _) => {
      println("Ping Request received at server2")
      for (i <- 1 to 5) {
        Thread.sleep(1000)
        println("ping processing")
      }

      sender ! HttpResponse(entity = "PiNG2")
    }

    case _ => {
      println("unknown message received" + context.sender().path);
    }
  }
}