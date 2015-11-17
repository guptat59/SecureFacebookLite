import java.util.Hashtable

import scala.concurrent.Await
import scala.util.Random

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.pattern.Patterns
import akka.pattern.ask
import akka.routing.SmallestMailboxPool
import akka.util.Timeout
import jsonProtocol.sprayJsonMarshaller
import jsonProtocol.sprayJsonUnmarshaller
import spray.http.MediaTypes
import spray.httpx.marshalling.ToResponseMarshallable.isMarshallable
import spray.routing.Directive.pimpApply
import spray.routing.Route
import spray.routing.SimpleRoutingApp

object FacebookServer extends App with SimpleRoutingApp {

  implicit val system = ActorSystem("FacebookServer")
  implicit val timeout = Timeout(10000)

  val routes = createProfiles ~
    addPost ~
    getPage ~
    getFrndList ~
    getProfiles ~
    createAlbum ~
    getAlbumInfo ~
    postPhoto ~
    getPhoto

  import jsonProtocol._

  startServer(interface = "localhost", port = 9443) {
    createProfiles ~
      getProfiles
  }

  val nrOfInstances: Int = 4

  val FBServers = system.actorOf(SmallestMailboxPool(nrOfInstances).props(Props(new FBServer())), name = "FB_Servers")

  def get_JsonRes(route: Route) = get {
    respondWithMediaType(MediaTypes.`application/json`) { route }
  }

  def post_JsonRes(route: Route) = post {
    respondWithMediaType(MediaTypes.`application/json`) { route }
  }

  lazy val createProfiles = {
    post_JsonRes {
      path("createusers") {
        entity(as[NewUsersReq]) { newUsr =>
          ctx => ctx.complete {
            Await.result(FBServers ? newUsr, timeout.duration).asInstanceOf[NewUsersRes];
          }
        }
      }
    }
  }

  lazy val getProfiles = {
    get_JsonRes {
      path("profile" / "[a-zA-Z0-9]*".r) { userId =>
        complete {
          println("getProfiles>>" + userId)
          var v = getUserProfile(userId)
          Await.result(FBServers ? v, timeout.duration).asInstanceOf[User];
        }
      }
    }
  }
  lazy val addPost = {
    get_JsonRes {
      post {
        path("user" / IntNumber / "feed") { userId =>
          //Todo
          //generate a UUID
          ctx => ctx.complete("post UUID added to all users")
        }
      }
    }
  }

  lazy val getPage = {
    get_JsonRes {
      get {
        path("user" / IntNumber / "home") { userId =>
          complete {
            //val future = Await.result(Patterns.ask(FBApi, getUserPage(userId.toString()), timeout), timeout.duration).asInstanceOf[String]
            //future
            ""
          }
        }
      }
    }
  }

  lazy val getFrndList = {
    get_JsonRes {
      get {
        path("friends" / IntNumber / "friendLists") { userId =>
          ctx => ctx.complete {
            //JSon array of user ids.
            "list"
          }
        }
      }
    }
  }

  lazy val createAlbum = {
    get_JsonRes {
      post {
        path("albums" / IntNumber / "create") { albumId =>
          complete {
            //Read the params, update album id.
            "done"
          }
        }
      }
    }
  }

  lazy val getAlbumInfo = {
    get_JsonRes {
      get {
        path("albums" / IntNumber) { albumId =>
          complete {
            //return the album info
            "done"
          }
        }
      }
    }
  }

  lazy val postPhoto = {
    get_JsonRes {
      post {
        path("albums" / IntNumber / "photos") { albumId =>
          complete {
            //save the new photo to db.
            "created pic"
          }
        }
      }
    }
  }

  lazy val getPhoto = {
    get_JsonRes {
      get {
        path("albums" / IntNumber / "photos" / IntNumber) { (albumId, photoId) =>
          complete {
            //send the photo.

            //extend to getPhotos if needed.
            "done"
          }
        }
      }
    }
  }

}

class FBServer extends Actor with ActorLogging {

  var userbase = new Hashtable[String, UserInfo]()

  def receive = {

    case newUsr: NewUsersReq => {
      var newIds = populateUserBase(newUsr)
      sender ! newIds
    }
    case gp: getUserPage => {
      var user = userbase.get(gp.username)
      sender ! user.getPosts()
    }
    case gpr: getUserProfile => {
      var user = userbase.get(gpr.userId)
      sender ! user.getPublicProfile()
    }
    case gfl: getFriendsList => {
      var user = userbase.get(gfl.username)
      sender ! user.getFriendList()
    }
  }

  private var userIdOffset = 0;
  def populateUserBase(newUsr: NewUsersReq): NewUsersRes = {
    var newIds = Array[String]()
    for (i <- 0 until newUsr.count) {
      //Create user with given data.
      var userId = newUsr.prefix + userIdOffset
      if (!userbase.containsKey(userId)) {
        var user = createUserWithID(userId)
        newIds = newIds :+ userId
        userbase.put(userId, user)
        userIdOffset += 1;
        println("User Created : " + user.getPublicProfile())
      } else {
        //Should not happen
        log.error("Duplicate user id generated : " + userId)
      }
    }
    NewUsersRes(newIds)
  }

  //Helper methods
  def createUserWithID(userId: String): UserInfo = {
    var user = new UserInfo(userId);
    user.insertData(Random.nextInt(100) + 1, "First-" + userId, "Last-" + userId, Gender.apply(Random.nextInt(Gender.maxId)).toString())
    user
  }
}

class UserInfo(val userid: String, var age: Int = -1, var firstName: String = "", var lastName: String = "", var gender: String = "NA") {

  var posts = new java.util.ArrayList[String]()
  var albumids = new java.util.ArrayList[String]()
  var friendlist = new java.util.ArrayList[String]()
  //var photoAlbum = new HashMap[String,photoids]()

  def insertData(a: Int, fn: String, ln: String, gen: String) {
    age = a
    firstName = fn
    lastName = ln
    gender = gen
  }

  def addPosts(post: String) {
    posts.add(post)
  }

  def getPosts(): String = {
    posts.toString()
  }

  def getPublicProfile(): User =
    {
      User(userid, firstName, lastName, age, gender);
    }

  def getFriendList(): String =
    {
      friendlist.toString()
    }
}

class Images {
  var bytecode = "": String
  var photoid = "": String
  var albumid = "": String
  def addPhoto(pid: String, bcode: String, aid: String) {
    bytecode = bcode
    photoid = pid
    albumid = aid
  }
}