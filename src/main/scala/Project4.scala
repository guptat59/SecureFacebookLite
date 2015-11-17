import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import spray.http.DateTime
import spray.http.MediaTypes
import spray.httpx.SprayJsonSupport
import spray.httpx.marshalling.ToResponseMarshallable.isMarshallable
import spray.json.DefaultJsonProtocol
import spray.routing.Directive.pimpApply
import spray.routing.Route
import spray.routing.SimpleRoutingApp
import spray.json.JsonFormat
import spray.json.RootJsonFormat
import spray.json.JsValue
import spray.json.JsObject
import spray.json.JsValue
import spray.json.JsArray
import spray.json.JsValue
import scala.concurrent.Await
import akka.util.Timeout
import scala.concurrent.impl.Future
import akka.dispatch.Futures
import akka.dispatch._
import scala.concurrent.Future
import akka.pattern.Patterns
import scala.concurrent.Await
import java.util.HashMap
import sun.security.pkcs11.Secmod
import java.util.Arrays.ArrayList
import java.util.Arrays.ArrayList
import java.util.concurrent.ConcurrentHashMap
import java.util.Arrays.ArrayList

object FacebookServer extends App with SimpleRoutingApp {

  implicit val system = ActorSystem("FacebookServer")
  var FBApi = system.actorOf(Props(new FacebookApi), name = "FBApi")
  var userlist = new HashMap[String,FBUser]()
  var user = new FBUser("1");
  userlist.put("1", user);
  user.createUser(22, "ab", "dv", "male")
  //creating bulk users
  /*for(i <- 0 to 100)
  {
     var user = new FBUser(i.toString());
    userlist.put(i.toString(), user);
    user.createUser(i, "a"+i, "b"+i, "male")
  }*/
  var list1 = new java.util.ArrayList[String]()
  user.addPosts("hi")
  user.addPosts("hello")
  user.addPosts("how are you")
  startServer(interface = "localhost", port = 8080) {

    get {
        path("home") { 
          complete {
            //JSon array of user ids.
            "return user profile."
          }
        }
      } ~
    createProfiles ~
      addPost ~
      getPage ~
      getFrndList ~
      getProfiles ~
      createAlbum ~
      getAlbumInfo ~
      postPhoto ~
      getPhoto
  }
  
  
  def getJson(route: Route) = get {
    respondWithMediaType(MediaTypes.`application/json`) { route }
  }

  lazy val createProfiles = {
    getJson {
      post {
        path("createusers") {
          ctx => ctx.complete("created")
        }
      }
    }
  }

  lazy val addPost = {
    getJson {
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
    getJson {
      get {
        path("user" / IntNumber / "home") { userId =>
          complete {
            //val future = user.getProfile()
            implicit val timeout = Timeout(100)
            val future = Await.result(Patterns.ask(FBApi, getUserPage(userId.toString()), timeout),timeout.duration).asInstanceOf[String]
            future
          }
        }
      }
    }
  }

  val getFrndList = {
    getJson {
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

  val getProfiles = {
    getJson {
      get {
        path("profile" / IntNumber) { userId =>
          complete {
            implicit val timeout = Timeout(30)
            val future = Await.result(Patterns.ask(FBApi, getUserProfile(userId.toString()), timeout),timeout.duration).asInstanceOf[String]
            future
          }
        }
      }
    }
  }

  val createAlbum = {
    getJson {
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

  val getAlbumInfo = {
    getJson {
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

  val postPhoto = {
    getJson {
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

  val getPhoto = {
    getJson {
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

  class FBUser(val userid: String) {
      var user_id: String = userid
      var age = 0: Int
      var firstname = "": String
      var lastname= "": String
      var gender= "": String
      var posts = new java.util.ArrayList[String]()
      var albumids = new java.util.ArrayList[String]()
      var friendlist = new java.util.ArrayList[String]()
      //var photoAlbum = new HashMap[String,photoids]()
      
      def createUser(a:Int,fn:String,sn:String,gen:String){
        age = a
        firstname = fn
        lastname = sn
        gender = gen
      }
      
       def addPosts(post:String){
        posts.add(post)
      }
       
       def getPosts():String ={
        posts.toString()
      }
       
       def getProfile():String ={
        var list = new java.util.ArrayList[String]()
        list.add(firstname)
        list.add(lastname)
        list.add(age.toString())
        list.add(gender)
        list.toString()
      }
       /* var home_page = new Array[String](120)
        var home_page_length = 0: Int
        var home_page_start = 0: Int // points to the oldest tweet if no of tweets are greater than 100.
        var tweets_received = 0: Int
*/
       def getFriendList():String =
       {
           friendlist.toString()
       }
    }
    
  class Images {
    var bytecode = "":String
    var photoid = "":String
    var albumid = "":String
    def addPhoto(pid:String,bcode:String,aid:String)
    {
     bytecode = bcode
     photoid = pid
     albumid = aid
    }
  }
  
  class Payloads {
    case class UserLogin(username: String, password: String)
    object UserLoginJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
      implicit val PortofolioFormats = jsonFormat2(UserLogin)
    }

    case class NewUsersReq(count: Int, prefix: String, suffixLength: Int)
    case class Post(message: String, link: String, place: String, privacy: String, object_attachment: String)
    case class UserPage(posts: Array[Post])

    object jsonProtocol extends DefaultJsonProtocol with SprayJsonSupport {
      implicit val NewUsersFormat = jsonFormat3(NewUsersReq)
      implicit val postFormat = jsonFormat5(Post)
      implicit object postList extends RootJsonFormat[UserPage] {
        def read(value: JsValue) = UserPage(value.convertTo[Array[Post]])
        def write(f: UserPage) = {
          JsArray.apply(f.posts.asInstanceOf[JsValue])
        }
      }
      implicit def userPageFormat[Post: JsonFormat] = jsonFormat1(UserPage.apply)
    }
  }

  sealed trait Seal
  case class newProfile(username: String, firstName: String, lastName: Option[String] = None, age: Int, birthday: Option[DateTime] = None, gender: String)
  case class getUserProfile(username: String)
  case class addFriend(user: FBUser)
  case class getUserPage(username: String)
  case class getFriendsList(username: String)
  case class getPhotos(username: String)
  case class getAlbums(username: String)

  class FacebookApi extends Actor {

    def receive = {
      case np: newProfile => {

      }
      case gp: getUserPage => {
       var user = FacebookServer.userlist.get(gp.username)
       
      
      // println(user.getPosts())
       sender ! user.getPosts()
       
      }
      case gpr: getUserProfile => {
       var user = FacebookServer.userlist.get(gpr.username)
        for(i <- 1 to 10)
       {
         println("qwerty"+ i)
         Thread.sleep(1000)
       }
       sender ! user.getProfile()
      }
      case gfl: getFriendsList => {
       var user = FacebookServer.userlist.get(gfl.username)
       sender ! user.getFriendList()
      }
    }
  }
}