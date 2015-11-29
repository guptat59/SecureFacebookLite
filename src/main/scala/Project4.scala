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
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import com.typesafe.scalalogging.slf4j.Logger
import java.util.ArrayList
import scala.collection.JavaConversions._
import scala.collection.immutable.TreeMap

object FacebookServer extends App with SimpleRoutingApp {

  implicit val system = ActorSystem("FacebookServer")
  implicit val timeout = Timeout(1000000)

  val routes = createProfiles ~
    createProfile ~
    getProfiles ~
    addFriend ~
    getFrndList ~
    addPost ~
    getPage ~
    createAlbum ~
    getAlbumInfo ~
    postPhoto ~
    initSystem

  import jsonProtocol._

  startServer(interface = "localhost", port = 8080) {
    routes
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

  var sysBoot = false
  lazy val initSystem = {
    post_JsonRes {
      path("systemboot") {
        complete {
          if (sysBoot) {
            Error("System is already set")
          } else {            
            val f = Await.result(FBServers ? systemSetup(), timeout.duration);            
            if (f.isInstanceOf[Success]) {
              sysBoot = true
              f.asInstanceOf[Success]
            } else if (f.isInstanceOf[Error])
              f.asInstanceOf[Error]
            else
              Error("Failed due to internal error2")
          }
        }
      }
    }
  }

  lazy val createProfiles = {
    post_JsonRes {
      path("createusers") {
        entity(as[NewUsersReq]) { newUsr =>
          complete {
            val f = Await.result(FBServers ? newUsr, timeout.duration);
            if (f.isInstanceOf[UsersList])
              f.asInstanceOf[UsersList]
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
            if (f.isInstanceOf[UsersList])
              f.asInstanceOf[UsersList]
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
            if (f.isInstanceOf[UsersList])
              f.asInstanceOf[UsersList]
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

      get {
        path("user" / "[a-zA-Z0-9]*".r / "friendslist" / "[a-zA-Z0-9]*".r) { (userId, frndId) =>
          complete {
            var f = Await.result(FBServers ? getFriendsList(userId, frndId), timeout.duration)
            if (f.isInstanceOf[UsersList]) {
              f.asInstanceOf[UsersList]
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

  lazy val createAlbum = {
    post_JsonRes {
      post {
        //have to pass json but the concept remains the same. albumid = albumname
        path("user" / "[a-zA-Z0-9]*".r / "albums" / "create") { (userId) =>
          entity(as[Album]) { album =>
            complete {
              if (userId.equals(album.userId)) {
                var f = Await.result(FBServers ? album, timeout.duration)
                if (f.isInstanceOf[Success]) {
                  f.asInstanceOf[Success]
                } else if (f.isInstanceOf[Error]) {
                  f.asInstanceOf[Error]
                } else {
                  Error("Failed due to internal error")
                }
              } else {
                Error(Constants.messages.noPermission)
              }
            }
          }
        }
      }
    }
  }

  lazy val getAlbumInfo = {
    get_JsonRes {
      get {
        path("albums" / "[a-zA-Z0-9]*".r) { userId =>
          complete {
            var f = Await.result(FBServers ? getUserAlbums(userId), timeout.duration)
            if (f.isInstanceOf[Array[Album]]) {
              f.asInstanceOf[Array[Album]]
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

  lazy val postPhoto = {
    post_JsonRes {
      post {
        //have to pass json but the concept remains the same. photo inside an album
        path("user" / "[a-zA-Z0-9]*".r / "albums" / "photo") { userId =>
          entity(as[Photo]) { photo =>
            complete {
              var f = Await.result(FBServers ? photo, timeout.duration)
              if (f.isInstanceOf[Success]) {
                f.asInstanceOf[Success]
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

    case su: systemSetup => {
      populateUsers()
      populateFriends()
      populatePosts()
      populatePhotoAlbums()
      println(userbase)
      for (i <- 1 to 100) {
        var userId = "user" + i
        if (userbase.contains("user" + i)) {
          var user = userbase.get(userId)
          println("I am " + userId)
          println("My friends " + user.get.friendList)
          println("My feed")

          for (u <- user.get.posts) {
            println(u.message)
          }
          println("My albums" + user.get.getAlbumInfo(userId + "album1"))
          println("My Photos")
          for (u <- user.get.getAlbumInfo(userId + "album1").photos) {
            println(u)
          }
          println("#####################")
        }
      }
      sender ! Success("Created users")
    }

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
        sender ! UsersList(newIds)
      } else {
        log.error("Duplicate user id generated : " + userId)
        sender ! Error(Constants.messages.userAlreadyPresent + userId)
      }
    }

    case fr: requestFriend => {
      var userId = fr.userId
      var frndId = fr.req.username
      if (userbase.contains(userId) && userbase.contains(frndId) && userId != frndId) {
        var user = userbase.get(userId)
        user.get.addFriend(frndId)
        var frnd = userbase.get(frndId)
        frnd.get.addFriend(frndId)
        var newIds = Array[String](frndId + "successfully added")
        sender ! UsersList(newIds)
      } else {
        log.error("Either of user or friend id is unavialable: " + userId + frndId)
        sender ! Error(Constants.messages.noUser + userId + " or " + frndId)
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

      if (user.isEmpty) {
        sender ! Error(Constants.messages.noUser)
      } else {
        if (gfl.frndId.isEmpty()) {
          // Send your own friend list
          sender ! UsersList(user.get.getFriendList().toList.toArray)
        } else {
          // Send your friends list
          if (user.get.friendList.contains(gfl.frndId) || gfl.userId.equals(gfl.frndId)) {
            var it = user.get.getFriendList().iterator
            var newIds = Array[String]()
            while (it.hasNext) {
              newIds = newIds :+ it.next()
            }
            sender ! UsersList(newIds)
          } else {
            sender ! Error(Constants.messages.noPermission)
          }
        }
      }
    }

    case aa: Album => {
      var user = userbase.get(aa.userId)
      if (user.isEmpty) {
        sender ! Error(Constants.messages.noUser)
      } else {
        var isSuccess = user.get.addAlbumToUser(aa.userId, aa);
        if (isSuccess) {
          //  Add this album permission to friends TODO
          sender ! Success(Constants.messages.albumCreated + aa.albumId)
        } else {
          sender ! Error(Constants.messages.albumCreationFailed)
        }
      }
    }

    case gai: getUserAlbums => {
      var user = userbase.get(gai.userId)
      if (user.isEmpty) {
        sender ! Error(Constants.messages.noUser)
      } else {

        sender ! user.get.getUserAlbums(gai.userId)
      }
    }

    case ai: Photo => {
      var user = userbase.get(ai.userId)
      if (user.isEmpty) {
        sender ! Error(Constants.messages.noUser)
      } else {
        var isSuccess = user.get.addPhotoToAlbum(ai.userId, ai.albumId, ai)
        if (isSuccess) {
          //  Add this photo permission to friends TODO
          sender ! Success(Constants.messages.photoAdded + ai.photoId)
        } else {
          sender ! Error(Constants.messages.photoAddFailed)
        }
      }
    }
  }

  private var userIdOffset = 0;
  def populateUserBase(newUsr: NewUsersReq): UsersList = {
    var newIds = Array[String]()
    for (i <- 0 until newUsr.count) {
      //Create user with given data.
      var userId = newUsr.prefix + userIdOffset
      if (!userbase.contains(userId)) {
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
    UsersList(newIds)
  }

  def populateUsers() {
    for (i <- 1 to Constants.totalUsers) {
      var userId = "user" + i
      var user = createUserWithID(userId)
      userbase.put(userId, user)
    }
  }

  def populatePosts() {
    for (i <- 1 to Constants.totalUsers) {
      var userId = "user" + i
      //if(userbase.contains(userId))
      //{
      var user = userbase.get(userId)
      var r = 1;
      while (r <= Constants.initialPostsCount) {
        var post = new UserPost(userId + "post" + r, None, None, "friends")
        var id = UUID.randomUUID().toString()
        user.get.addToFeed(id, post)
        r = r + 1
        var it = user.get.getFriendList().iterator
        while (it.hasNext) {
          var friendId = it.next()
          var friend = userbase.get(friendId)
          friend.get.addToFeed(id, post)
        }
      }
      /* }
        else {
          println(userId+ " not found")
        }*/
    }
  }

  def populateFriends() {
    for (i <- 1 to Constants.totalUsers) {
      var userId = "user" + i
      /*if(userbase.contains(userId))
        {*/
      var user = userbase.get(userId)
      var j = 1
      while (j <= Constants.numOfFriends) {
        var r = Random.nextInt(Constants.totalUsers)
        if (!(user.get.friendList.contains("user" + r)) && r != i) {
          var frnd = userbase.get("user" + r)
          user.get.addFriend("user" + r)
          frnd.get.addFriend(userId)
          j = j + 1
        }
      }
      // }
    }
  }

  def populatePhotoAlbums() {
    for (i <- 1 to Constants.totalUsers) {
      var userId = "user" + i
      /* if (userbase.contains("user"+i))
          {
            */
      var user = userbase.get(userId)
      var j = 1;
      while (j <= Constants.initialAlbumsCount) {
        var list = Array("photo1", "photo2", "photo3")
        var photo = new Album(userId, userId + "album" + j, None, None, None, None, None, list)
        user.get.addAlbumToUser(userId, photo);
        j = j + 1
      }
      //}
    }
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
  var friendList = new ListBuffer[String]()
  val log = Logger(LoggerFactory.getLogger("UserInfo"))

  private var userAlbums = new ListBuffer[String]
  private var albumStore = new HashMap[String, PictureAlbum]
  private var photoStore = new HashMap[String, Picture]

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

  def addAlbumToUser(userId: String, album: Album): Boolean = {

    //Add this album to user album store.
    if (userAlbums.contains(album.albumId)) {
      // Album already exists for this user. Should not happen. Higher level check
      log.error("Album already added!!")
      false
    } else {
      userAlbums.append(album.albumId);
      var picAlbum = new PictureAlbum(userId, album.albumId)
      albumStore.put(album.albumId, picAlbum)
      true
    }
  }

  def addPhotoToAlbum(userId: String, albumId: String, photo: Photo): Boolean = {
    // Need not check userId, albumId. Higher level check
    if (photoStore.get(photo.photoId) == null) {
      if (albumStore.get(albumId) == null) {
        //TODO return error, should not happen
        log.error("Album not found")
        false
      } else {
        // When security is implemented, content will be encrypted so it doesn't not matter to validate.
        var album: PictureAlbum = albumStore.get(albumId)
        var pic = new Picture(userId, albumId, photo.src);
        pic.populate(photo.message, photo.place)
        album.addPicture(pic)
        photoStore.put(pic.photoId, pic)
        true
      }
    } else {
      //Photo already exists. Nothing to do
      log.error("Photo already exists!!")
      false
    }
  }

  def getUserAlbumsIds(userId: String): ListBuffer[String] = {
    userAlbums
  }

  def getAlbumInfo(albumId: String): Album = {
    var a = albumStore.get(albumId)
    var b = new Album(a.ownerId, a.albumId, a.coverPhoto, Option(a.createdTime), a.description, a.place, Option(a.updateTime), a.photos.toList.toArray)
    b
  }

  def getUserAlbums(userId: String): Array[Album] = {
    var a = userAlbums;
    var it = a.iterator
    var albums = Array[Album]()
    while (it.hasNext) {
      var albumId = it.next()
      if (albumId != null)
        albums = albums :+ getAlbumInfo(albumId)
    }
    albums
  }

  def getUserAlbumPhotos(userId: String, albumId: String): Array[Photo] = {
    // Need not verify userId, albumId as content will be encrypted
    var a = albumStore.get(albumId);
    var it = a.photos.iterator
    var pics = Array[Photo]()
    while (it.hasNext) {
      var picId = it.next()
      if (picId != null)
        pics = pics :+ getUserAlbumPhoto(userId, albumId, picId)
    }
    pics
  }

  def getUserAlbumPhoto(userId: String, albumId: String, photoId: String): Photo = {
    //Need not verify userId, albumId & photoId as content will be encrypted
    var p = photoStore.get(photoId)
    new Photo(userId, albumId, p.photoId, p.src, p.message, p.place, p.nostory)
  }

}