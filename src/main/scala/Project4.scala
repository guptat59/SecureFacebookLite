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
import scala.collection.mutable.ListBuffer
import java.util.UUID
import scala.collection.parallel.mutable.ParHashMap
import spray.httpx.marshalling.ToResponseMarshaller
import akka.routing.RoundRobinRouter
import akka.routing.SmallestMailboxRouter
import akka.routing.SmallestMailboxPool
import java.util.HashMap
import java.util.Arrays.ArrayList

object FacebookServer extends App with SimpleRoutingApp {

  implicit val system = ActorSystem("FacebookServer")
  implicit val timeout = Timeout(1000000)

  val routes = createProfiles ~
    createProfile ~
    addPost ~
    addFriend ~
    getPage ~
    getFrndList ~
    getProfiles ~
    createAlbum ~
    getAlbumInfo ~
    postPhoto ~
    getPhoto

  import jsonProtocol._

  startServer(interface = "localhost", port = 8080) {
    createProfiles ~
    createProfile ~
      getProfiles ~
      addFriend ~
      getFrndList ~
      addPost ~
      getPage ~
      createAlbum ~
      postPhoto
  }

  val nrOfInstances: Int = 4

  val FBServers = system.actorOf(SmallestMailboxPool(nrOfInstances).props(Props(new FBServer())), name = "FB_Servers")
  //val FBServers = system.actorOf( Props(new FBServer()).withRouter(SmallestMailboxRouter(5)), name = "FB_Servers")
  def buildaddPost(userId: String, post: UserPost): addPost = {
    var p = new addPost(UUID.randomUUID().toString(), userId, post)
    p
  }

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
          complete {
            val f = Await.result(FBServers ? newUsr, timeout.duration);
            if (f.isInstanceOf[NewUsersRes])
              f.asInstanceOf[NewUsersRes]
            else if (f.isInstanceOf[Error])
              f.asInstanceOf[Error]
            else
              Error("Failed due to internal error2")
          }
        }
      }
    }
  }
  
  lazy val createProfile = {
    post_JsonRes {
      path("createuser") {
        entity(as[NewUserReq]) { newUsr =>
          complete {
            val f = Await.result(FBServers ? newUsr, timeout.duration);
            if (f.isInstanceOf[NewUsersRes])
              f.asInstanceOf[NewUsersRes]
            else if (f.isInstanceOf[Error])
              f.asInstanceOf[Error]
            else
              Error("Failed due to internal error2")
          }
        }
      }
    }
  }
  lazy val getProfiles = {
    get_JsonRes {
      path("profile" / "[a-zA-Z0-9]*".r) { userId =>
        complete {
          val f = Await.result(FBServers ? findProfile(userId), timeout.duration)
          if (f.isInstanceOf[User]) {
            f.asInstanceOf[User]
          } else if (f.isInstanceOf[Error]) {
            f.asInstanceOf[Error]
          } else {
            Error("Failed due to internal error")
          }
        }
      }
    }
  }

  lazy val addPost = {
    post_JsonRes {
      path("user" / "[a-zA-Z0-9]*".r / "feed") { userId =>
        //Verify the user. TODO        
        entity(as[UserPost]) { postjson =>
          complete {
            val f = Await.result(FBServers ? buildaddPost(userId, postjson), timeout.duration)
            if (f.isInstanceOf[PostAdded]) {
              f.asInstanceOf[PostAdded]
            } else if (f.isInstanceOf[Error]) {
              f.asInstanceOf[Error]
            } else {
              Error("Failed due to internal error")
            }
          }
        }
      }
    }
  }

  lazy val getPage = {
    get_JsonRes {
      path("user" / "[a-zA-Z0-9]*".r / "home") { userId =>
        complete {
          var f = Await.result(FBServers ? getUserPage(userId), timeout.duration)
          if (f.isInstanceOf[UserPage]) {
            f.asInstanceOf[UserPage]
          } else if (f.isInstanceOf[Error]) {
            f.asInstanceOf[Error]
          } else {
            Error("Failed due to internal error")
          }
        }
      }
    }
  }

  lazy val addFriend = {
    post_JsonRes {
      path("user" / "[a-zA-Z0-9]*".r / "addfriend") { userId =>
        entity(as[FriendReq]) { frndreq =>
          complete {
            val f = Await.result(FBServers ? requestFriend(userId, frndreq), timeout.duration);
            if (f.isInstanceOf[NewUsersRes])
              f.asInstanceOf[NewUsersRes]
            else if (f.isInstanceOf[Error])
              f.asInstanceOf[Error]
            else
              Error("Failed due to internal error2")
          }
        }
      }
    }
  }
  lazy val getFrndList = {
    get_JsonRes {
      path("user" / "[a-zA-Z0-9]*".r / "friendslist" / "[a-zA-Z0-9]*".r) { (userId,frndId) =>
        complete {
          var f = Await.result(FBServers ? getFriendsList(userId,frndId), timeout.duration)
          if (f.isInstanceOf[NewUsersRes]) {
            f.asInstanceOf[NewUsersRes]
          } else if (f.isInstanceOf[Error]) {
            f.asInstanceOf[Error]
          } else {
            Error("Failed due to internal error")
          }
        }
      }
    }
  }

  lazy val createAlbum = {
    post_JsonRes {
      post {
        //have to pass json but the concept remains the same. albumid = albumname
        path("user"/"[a-zA-Z0-9]*".r /"albums" / "[a-zA-Z0-9]*".r  / "create") { (userId,albumId) =>
          complete {
          var f = Await.result(FBServers ? addAlbums(userId,albumId), timeout.duration)
          if (f.isInstanceOf[NewUsersRes]) {
            f.asInstanceOf[NewUsersRes]
          } else if (f.isInstanceOf[Error]) {
            f.asInstanceOf[Error]
          } else {
            Error("Failed due to internal error")
          }
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
    post_JsonRes {
      post {
        //have to pass json but the concept remains the same. photo inside an album
        path("user"/"[a-zA-Z0-9]*".r /"albums" / "[a-zA-Z0-9]*".r  / "photo"/"[a-zA-Z0-9]*".r ) { (userId,albumId,photoId) =>
          complete {
          var f = Await.result(FBServers ? addPhoto(userId,albumId,photoId), timeout.duration)
          if (f.isInstanceOf[NewUsersRes]) {
            f.asInstanceOf[NewUsersRes]
          } else if (f.isInstanceOf[Error]) {
            f.asInstanceOf[Error]
          } else {
            Error("Failed due to internal error")
          }
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

  var userbase = new ParHashMap[String, UserInfo]()

  def receive = {

    case newUsr: NewUsersReq => {
      var newIds = populateUserBase(newUsr)
      sender ! newIds
    }
    case singleUsr: NewUserReq => {
      var userId = singleUsr.username
      if (!userbase.contains(userId)) {
        var user = createUserWithID(userId)
        userbase.put(userId, user)
        var newIds = Array[String](userId)
        sender ! NewUsersRes(newIds)
      }
      else {
        log.error("Duplicate user id generated : " + userId)
        var newIds = Array[String]("Username already exists")
        sender ! NewUsersRes(newIds)
      }
    }
    case fr: requestFriend => {
      var userId = fr.userId
      var frndId = fr.req.username
      if (userbase.contains(userId) && userbase.contains(frndId) ) {
        var user = userbase.get(userId)
        user.get.addFriend(frndId)
        var frnd = userbase.get(frndId)
        frnd.get.addFriend(frndId)
        var newIds = Array[String](frndId+"successfully added")
        sender ! NewUsersRes(newIds)
      }
      else {
        log.error("Either of user or friend id is unavialable: " + userId + frndId)
        var newIds = Array[String]("Username doesnt exist")
        sender ! NewUsersRes(newIds)
      }
    }
    case gpr: findProfile => {
      var user = userbase.get(gpr.userId)
      if (user.isEmpty) {
        sender ! Error(Constants.messages.noUser)
      } else {
        sender ! user.get.getPublicProfile()
      }
    }
    case ap: addPost => {
      var user = userbase.get(ap.userId)
      if (user.isEmpty) {
        sender ! Error(Constants.messages.noUser)
      } else {
        //Can control this based on a private post or friends post.
        ap.post.privacy match {
          case Constants.privacy.personal => {
            user.get.addToFeed(ap.postId, ap.post)
          }
          case Constants.privacy.friends =>
            {
              user.get.addToFeed(ap.postId, ap.post)
              var it = user.get.getFriendList().iterator
              while (it.hasNext) {
                var friendId = it.next()
                var friend = userbase.get(friendId)
                friend.get.addToFeed(ap.postId, ap.post)
              }
            }
            sender ! PostAdded(ap.postId, Constants.messages.success)
        }
      }
    }
    case gp: getUserPage => {
      var user = userbase.get(gp.userId)
      if (user.isEmpty) {
        sender ! Error(Constants.messages.noUser)
      } else {
        sender ! user.get.getPosts()
      }
    }
    case gfl: getFriendsList => {
      var user = userbase.get(gfl.userId)
      if(user.get.friendList.contains(gfl.frndId) || gfl.userId.equals(gfl.frndId))
      {
        var it = user.get.getFriendList().iterator
        var newIds = Array[String]()
         while (it.hasNext) {
                newIds = newIds :+ it.next()
        }
        sender ! NewUsersRes(newIds)
      }
      else {
        var newIds = Array[String]("You dont have permission to view his friendlist")
        sender ! NewUsersRes(newIds)
      }
    }
    case aa : addAlbums => {
      var user = userbase.get(aa.userId)
      user.get.createAlbum(aa.albumId);
      var ids = Array[String]()
      var id = aa.albumId
      ids = ids :+ id  
      sender ! NewUsersRes(ids)
    }
    case ai : addPhoto => {
      var user = userbase.get(ai.userId)
      user.get.addPhoto(ai.userId,ai.albumId,ai.photoId);
      var ids = Array[String]()
      var id = ai.photoId
      ids = ids :+ id  
      sender ! NewUsersRes(ids)
    }
  }

  private var userIdOffset = 0;
  def populateUserBase(newUsr: NewUsersReq): NewUsersRes = {
    var newIds = Array[String]()
    for (i <- 0 until newUsr.count) {
      //Create user with given data.
      var userId = newUsr.prefix + userIdOffset
      if (!userbase.contains(userId)) {
        var user = createUserWithID(userId)
        newIds = newIds :+ userId
        userbase.put(userId, user)
        userIdOffset = i;
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

  /**
   * Listbuffer internally uses list
   * head - Constant time
   * tail - Linear time
   * apply - Linear time
   * update - Linear time
   * prepend - Constant time
   * append - Constant time
   * insert - Linear time
   * toList - Constant time
   */
  var posts = new ListBuffer[UserPost]()

  var albumids = new ListBuffer[String]()
  var friendList = new ListBuffer[String]()
  var photoAlbum = new HashMap[String,java.util.ArrayList[String]]()

  def insertData(a: Int, fn: String, ln: String, gen: String) {
    age = a
    firstName = fn
    lastName = ln
    gender = gen
  }

  def addToFeed(postId: String, post: UserPost) = {
    posts.prepend(post)
  }

  def getPosts(): UserPage = {
    return UserPage(posts.toList.toArray)
  }

  def addFriend(friendId: String) = {
    friendList.append(friendId)
  }

  def getPublicProfile(): User = {
    User(userid, firstName, lastName, age, gender);
  }

  def getFriendList(): ListBuffer[String] = {
    friendList
  }
  
  def createAlbum(albumId :String) {
    albumids.append(albumId)
  }
  
  def addPhoto(userId:String,albumId :String,photoId: String) {
    //after json is sent updates Images class with photoid and photoid is stored below.
    if(photoAlbum.containsKey(albumId))
    {
      var list = photoAlbum.get(albumId)
      list.add(photoId);
      photoAlbum.put(albumId, list)
    }
    else
    {
      var list = new java.util.ArrayList[String]()
      list.add(photoId)
      photoAlbum.put(albumId, list)
    }
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