import akka.actor._
import spray.http._
import spray.can.Http
import akka.io.IO
import spray.routing.SimpleRoutingApp

object FacebookServer extends App with SimpleRoutingApp{


    implicit val system = ActorSystem()
    
    // type Route = RequestContext => Unit
    
    // directives
    
    startServer(interface  = "localhost", port = 9443) {
      get {
        path("hello") {          
          complete {
            "welcome to home page!!"
          }
        }
      }
    }
    
    // var ref: ActorRef = system.actorOf(Props[Actor])
    // IO(Http) ! Http.Bind(ref, interface = "localhost", port = 9443)


}

class act extends Actor {

  var httpListener: ActorRef = _

  def receive = {

    case y: Http.Bound => {
      httpListener = sender()
    }

    case "Stop" => {

    }

    case HttpRequest(HttpMethods.GET, Uri.Path("/ping"), _, _, _) => {
      println("Request received at server")
      sender ! HttpResponse(entity = "PONG")
    }
  }
}