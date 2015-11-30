import java.nio.file.Files
import java.nio.file.Paths
import java.util.HashMap
import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.mutable.ParHashMap
import scala.concurrent.Await
import scala.util.Random
import org.apache.commons.codec.binary.Base64
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.slf4j.Logger
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
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
import spray.routing.directives.DetachMagnet.fromUnit
import com.typesafe.config.ConfigFactory

object FacebookServer extends App with SimpleRoutingApp {

  val actorConf = """
  akka {
    #log-config-on-start = on
    stdout-loglevel = "INFO"
    loglevel = "DEBUG"    
  }
  """

  implicit val system = ActorSystem("FacebookServer", ConfigFactory.parseString(actorConf))
  implicit val timeout = Timeout(1000000)

  val routes =
    createProfile ~
      getProfiles ~
      addFriend ~
      getFrndList ~
      addPost ~
      getPage ~
      createAlbum ~
      getAlbumInfo ~
      postPhoto

  import jsonProtocol._

  val nrOfInstances: Int = 10
  val FBServers = system.actorOf(SmallestMailboxPool(nrOfInstances).props(Props(new FBServer())), name = "FB_Servers")

  startServer(interface = Constants.serverHost, port = Constants.serverPort) {
    routes
  }

  def buildaddPost(userId: String, post: UserPost): addPost = {
    var p = new addPost(UUID.randomUUID().toString(), userId, post)
    p
  }

  def jsonRes(route: Route) = {
    respondWithMediaType(MediaTypes.`application/json`) { detach() { route } }
  }

  def get_JsonRes(route: Route) = get {
    respondWithMediaType(MediaTypes.`application/json`) { detach() { route } }
  }

  def post_JsonRes(route: Route) = post {
    respondWithMediaType(MediaTypes.`application/json`) { detach() { route } }
  }

  lazy val createProfile = {
    jsonRes {
      path("user") {
        put {
          entity(as[User]) { newUsr =>
            complete {
              // TODO
              val f = Await.result(FBServers ? newUsr, timeout.duration);
              if (f.isInstanceOf[Success])
                f.asInstanceOf[Success]
              else if (f.isInstanceOf[Error])
                f.asInstanceOf[Error]
              else
                Error("Failed due to internal error2")
            }
          }
        } ~ post {
          entity(as[User]) { newUsr =>
            complete {
              val f = Await.result(FBServers ? newUsr, timeout.duration);
              if (f.isInstanceOf[Success])
                f.asInstanceOf[Success]
              else if (f.isInstanceOf[Error])
                f.asInstanceOf[Error]
              else
                Error("Failed due to internal error2")
            }
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
        entity(as[FriendRequest]) { frndreq =>
          complete {
            val f = Await.result(FBServers ? frndreq, timeout.duration);
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
    jsonRes {
      put {
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
      } ~
        delete {
          complete {
            Error("TODO")
          }
        } ~
        post {
          complete {
            Error("TODO")
          }
        }
    }
  }

  lazy val getAlbumInfo = {
    jsonRes {
      path("user" / "[a-zA-Z0-9]*".r / "albums") { userId =>
        get {
          parameters("frndId"?) { frndId =>
            complete {
              var f = Await.result(FBServers ? getUserAlbums(userId, frndId), timeout.duration)
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
  }

  lazy val postPhoto = {
    jsonRes {

      put {
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
      } ~
        post {
          complete {
            Error("TODO")
          }
        } ~
        delete {
          complete {
            Error("TODO")
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

  var userbase = new ParHashMap[String, UserInfo]()

  class FBServer extends Actor with ActorLogging {

    def receive = {

      case u: User => {
        var userId = u.userId
        if (!userbase.contains(userId)) {
          var user = createUserWithID(u)
          userbase.put(userId, user)
          log.debug("created user : " + userId)
          sender ! Success(Constants.messages.created + userId)
        } else {
          log.error("Duplicate user id generated : " + userId)
          sender ! Error(Constants.messages.userAlreadyPresent + userId)
        }
      }

      case fr: FriendRequest => {
        var userId = fr.userId
        var frndIds = fr.frndId.split(" , ")
        if (userbase.contains(userId)) {
          var newIds = Array[String]()
          var user = userbase.get(userId)
          var it = frndIds.iterator
          while (it.hasNext) {
            var frndId = it.next().trim()
            log.debug("Making friends : " + userId + " , " + frndId)
            if (!frndId.isEmpty() && userId != frndId && userbase.contains(frndId)) {
              user.get.addFriend(frndId)
              var frnd = userbase.get(frndId)
              frnd.get.addFriend(frndId)
            }
            newIds = newIds :+ frndId
          }
          sender ! UsersList(newIds)
        } else {
          log.error("Either of user or friend id is unavialable: " + userId + frndIds)
          sender ! Error(Constants.messages.noUser + userId + " or " + frndIds)
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
          if (ap.post.privacy.equals(Constants.Privacy.Private)) {
            user.get.addToFeed(ap.postId, ap.post)
          } else if (ap.post.privacy.equals(Constants.Privacy.Friends)) {
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
          if (gai.userId.equals(gai.frndId)) {
            sender ! user.get.getUserAlbums(gai.userId)
          } else if (!gai.frndId.isEmpty && userbase.contains(gai.frndId.get)) {
            sender ! user.get.getUserAlbums(gai.frndId.get)
          }
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

    //Helper methods
    def createUserWithID(u: User): UserInfo = {
      var user = new UserInfo(u.userId);
      user.insertData(u.age, u.firstName, u.lastName, u.gender)
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
      var b = new Album(a.ownerId, a.albumId, a.coverPhoto, Option(a.createdTime), a.description, a.place, Option(a.updateTime), Option(a.photos.toList.toArray))
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
}