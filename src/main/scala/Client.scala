import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.Random
import com.typesafe.config.ConfigFactory
import Constants.Privacy
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorSelection.toScala
import akka.actor.ActorSystem
import akka.actor.Cancellable
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.pattern.ask
import akka.util.Timeout
import spray.client.pipelining.Get
import spray.client.pipelining.Post
import spray.client.pipelining.sendReceive
import spray.http.ContentType.apply
import spray.http.HttpEntity
import spray.http.HttpResponse
import spray.http.MediaTypes
import spray.httpx.SprayJsonSupport
import spray.json.AdditionalFormats
import java.nio.file.Paths
import java.nio.file.Files
import org.apache.commons.codec.binary.Base64
import java.util.concurrent.atomic.AtomicInteger
import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import spray.json.JsValue
import spray.json.JsonFormat
import spray.json.NullOptions
import spray.json.RootJsonFormat
import spray.json.pimpAny
import java.util.HashMap
import spray.json.AdditionalFormats
import spray.httpx.RequestBuilding._
import spray.http._
import HttpMethods._
import HttpHeaders._
import ContentTypes._
import scala.concurrent.Await

object FacebookSimulator {

  var namingPrefix = "akka://" + "FBClients" + "/user/"
  val actorConf = """
  akka {
    #log-config-on-start = on
    stdout-loglevel = "DEBUG"
    loglevel = "DEBUG"    
  }
  """
  val system = ActorSystem("FBClients", ConfigFactory.parseString(actorConf))
  implicit val timeout = Timeout(1)

  val userPrefix = "user"

  def main(args: Array[String]) {
    //create 100 users
    //add 10 friends to each user
    //20-30% users post/add photo/album
    //70% gets friendslist/page of friends/profile of friends/photos/albums
    //user should request for 
    //FBServer.
    //1.5billion users
    //1 billion active users
    //600 million posts
    //300 million photos

    createUsers()

    while (createdUsers.get() < Constants.totalUsers) {
      Thread.sleep(1000)
      println("Waiting till all the users have booted up!! : " + createdUsers.get())
    }

    Thread.sleep(1000)
    makeFriends()

    while (frndsAdded.get() < Constants.totalUsers) {
      Thread.sleep(1000)
      println("Waiting till all the users have made some friends!! : " + frndsAdded.get())
    }

    Thread.sleep(1000)

    var firstTime = true;

    var userName = userPrefix + (Random.nextInt(Constants.totalUsers) + 1)
    var frndName = userPrefix + (Random.nextInt(Constants.totalUsers) + 1)
    val sleepdelay = 1000

    // Sanity Check
    while (true) {
      Thread.sleep(5000)
      println("##########################################################################################################")
      var user = system.actorSelection(namingPrefix + userName)
      var frnd = system.actorSelection(namingPrefix + frndName)

      if (firstTime) {
        user ? addFriend(frndName)
        firstTime = false
      }

      println("UserPost >> ")

      user ? UserPost("post by " + userName, None, None, Privacy.Friends, None); Thread.sleep(sleepdelay)
      frnd ? UserPost("post by " + frndName, None, None, Privacy.Friends, None); Thread.sleep(sleepdelay)
      if (false) {
        println("get profile >> ")

        user ? getProfile(frndName); Thread.sleep(sleepdelay)
        frnd ? getProfile(userName); Thread.sleep(sleepdelay)

        println("get page >> ")

        user ? getPage(); Thread.sleep(sleepdelay)
        frnd ? getPage(); Thread.sleep(sleepdelay)

        println("get friends list>> ")

        user ? getFriendsList(userName, frndName); Thread.sleep(sleepdelay)
        frnd ? getFriendsList(frndName, userName); Thread.sleep(sleepdelay)

        println("get album info>> ")

        user ? getAlbumsInfo(); Thread.sleep(sleepdelay)
        frnd ? getAlbumsInfo(); Thread.sleep(sleepdelay)

      }
      Thread.sleep(10000)
    }

    startSchedulers()

  }

  var createdUsers = new AtomicInteger(0);
  var frndsAdded = new AtomicInteger(0);

  def createUsers(): Unit = {

    for (i <- 0 to Constants.totalUsers) {
      var userId = userPrefix + i
      var user = system.actorOf(Props(new UserClient(userId)), userId)
      var u = new User(userId, "First-" + userId, "Last-" + userId, Random.nextInt(100) + 1, Gender.apply(Random.nextInt(Gender.maxId)).toString());
      user ! u
    }
  }

  def makeFriends(): Unit = {

    println("Per user frnds : " + Constants.numOfFriends)

    for (i <- 0 to Constants.totalUsers) {
      var userId = userPrefix + i
      var user = system.actorSelection(namingPrefix + userId)

      var frndIds = Array[String]()

      var frndCount = 1
      while (frndCount < Constants.numOfFriends) {
        var frndId = userPrefix + ((i + frndCount) % Constants.totalUsers)
        frndIds = frndIds :+ frndId
        frndCount = frndCount + 1
      }
      var fList = frndIds.mkString(",")
      println("frndIds : " + frndIds.length + fList)

      var fr = new FriendRequest(userId, fList);
      user ! fr
    }
  }

  def startSchedulers(): Unit = {
    import system.dispatcher

    var postGenerator: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1.toLong, "seconds"), FiniteDuration.apply(.1.toLong, "seconds"), (new Runnable {
      def run {
        var userName = userPrefix + (Random.nextInt((Constants.totalUsers * 10) / 100) + 1)
        println(userName + " scheduler1")
        var user = system.actorSelection(namingPrefix + userName)
        user ! UserPost("post", None, None, Privacy.Friends, None)
      }
    }))

    var albumGenerator: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1.toLong, "seconds"), FiniteDuration.apply(.1.toLong, "seconds"), (new Runnable {
      def run {
        var userName = userPrefix + (Random.nextInt((Constants.totalUsers * 10) / 100) + 1)
        println(userName + " scheduler1")
        var user = system.actorSelection(namingPrefix + userName)
        user ! UserPost("post", None, None, Privacy.Friends, None)
      }
    }))

    var sccount = 1
    var imageNAlbumGenerator: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1, "seconds"), FiniteDuration.apply(.2.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt((Constants.totalUsers * 20) / 100) + 1
        var userName = userPrefix + r
        println(userName + " scheduler2")
        var user = system.actorSelection(namingPrefix + userName)
        if (r % 2 == 0)
          user ! new Album(userName, userName + "album" + sccount)
        else
          user ! new Photo(userName, userName + "album1", "uphoto" + sccount, "fb", None, None, true)
      }
    }))

    var pageNProfileFetcher: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1, "seconds"), FiniteDuration.apply(.001.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt((Constants.totalUsers * 80) / 100) + 1 + (Constants.totalUsers * 20) / 100
        var userName = userPrefix + r
        println(userName + " scheduler3")
        var user = system.actorSelection(namingPrefix + userName)
        if (r % 2 == 0)
          user ! getProfile(userPrefix + Random.nextInt(Constants.totalUsers))
        else
          user ! getPage()
      }
    }))

    var friendRequester: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1, "seconds"), FiniteDuration.apply(5.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt(Constants.totalUsers) + 1
        var userName = userPrefix + r
        println(userName + " scheduler4")
        var user = system.actorSelection(namingPrefix + userName)
        user ! addFriend(userPrefix + (Random.nextInt(Constants.totalUsers) + 1))
      }
    }))

    postGenerator.cancel()
    imageNAlbumGenerator.cancel()
    pageNProfileFetcher.cancel()
    friendRequester.cancel()

  }

  sealed trait seal

  case class getPage()
  case class getProfile(userId: String)
  case class getFriendList(userId: String, frndId: String)
  case class getAlbumsInfo()

  class UserClient(userId: String) extends Actor with SprayJsonSupport with AdditionalFormats with ActorLogging {
    implicit val system = context.system
    import system.dispatcher
    implicit val timeout = Timeout(1000000)

    val pipeline = (
      sendReceive)

    def receive = {
      //(userId: String, firstName: String, lastName: String, age: Int, gender: String)
      case u: User =>
        {

          var userId = u.userId
          var firstName = u.firstName
          var lastName = u.lastName
          var age = u.age
          var gender = u.gender
          log.debug("Creating user with id " + userId)
          val result: Future[HttpResponse] = pipeline(Post(Constants.serverURL + "/createuser", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "firstName" : "$firstName" , "lastName" : "$lastName", "age" : $age , "gender" :  "$gender"}""")))
          Await.result(result, timeout.duration)
          result.onComplete {
            x =>
              {
                createdUsers.incrementAndGet()
                x.foreach { res => log.debug(res.entity.asString) }
              }
          }
        }

      case rf: FriendRequest => {
        var userId = rf.userId
        var frndIds = rf.frndId
        log.debug("Requesting user,frnd : " + userId + " , " + frndIds)
        val result: Future[HttpResponse] = pipeline(Post(Constants.serverURL + "/user/" + userId + "/addfriend", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId" , "frndId" : "$frndIds" }""")))
        Await.result(result, timeout.duration)
        result.onComplete {
          x =>
            {
              frndsAdded.incrementAndGet()
              x.foreach { res => log.debug(res.entity.asString) }
            }
        }
      }

      case ap: UserPost => {
        val result: Future[HttpResponse] = pipeline(Post(Constants.serverURL + "/user/" + userId + "/feed", HttpEntity(MediaTypes.`application/json`, s"""{"message": "$ap.message", "link": "$ap.link", "place": "$ap.place", "privacy": "$ap.privacy", "object_attachment": "$ap.object_attachment"}""")))
        Await.result(result, timeout.duration)
        result.onComplete {
          x =>
            {
              frndsAdded.incrementAndGet()
              x.foreach { res => log.debug(res.entity.asString) }
            }
        }
      }

      case gp: getProfile => {
        val result: Future[HttpResponse] = pipeline(Get(Constants.serverURL + "/profile/" + gp.userId))
        Await.result(result, timeout.duration)
        result.onComplete {
          x =>
            {
              frndsAdded.incrementAndGet()
              x.foreach { res => log.debug(res.entity.asString) }
            }
        }
      }

      case gup: getPage => {
        val result: Future[HttpResponse] = pipeline(Get(Constants.serverURL + "/user/" + userId + "/home"))
        Await.result(result, timeout.duration)
        result.onComplete {
          x =>
            {
              frndsAdded.incrementAndGet()
              x.foreach { res => log.debug(res.entity.asString) }
            }
        }
      }

      case gfl: getFriendList => {
        var userID = gfl.userId
        var frndId = gfl.frndId
        val result: Future[HttpResponse] = pipeline(Get(Constants.serverURL + "/user/" + userID + "/friendslist/" + frndId))
        Await.result(result, timeout.duration)
        result.onComplete {
          x =>
            {
              frndsAdded.incrementAndGet()
              x.foreach { res => log.debug(res.entity.asString) }
            }
        }
      }

      case a: Album => {
        var userId = a.userId
        var albumId = a.albumId
        var coverPhoto = a.coverPhoto
        var createdTime = a.createdTime
        var description = a.description
        var place = a.place
        var updateTime = a.updateTime
        var photos = a.photos
        log.info(photos.mkString(","))
        var photostring = a.photos.mkString(",")
        val result: Future[HttpResponse] = pipeline(Post(Constants.serverURL + "/user/" + userId + "/albums/create", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "albumId" :"$albumId", "coverPhoto" : "$coverPhoto", "createdTime" : "$createdTime", "description" : "$description", "place":"$place", "updateTime" :"$updateTime", "photos" : [""]}""")))
        result.foreach {
          response =>
            log.info(s"added album:\n${response.entity.asString}")
        }
      }

      case p: Photo => {
        var userId = p.userId
        var albumId = p.albumId
        var message = p.message
        var noStory = p.noStory
        var photoId = p.photoId
        var place = p.place
        var src = p.src
        val result: Future[HttpResponse] = pipeline(Post(Constants.serverURL + "/user/" + userId + "/albums/photo", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "albumId" : "$albumId", "place": "$place","photoId": "$photoId", "src": "$src", "message": "$message", "noStory": $noStory}""")))
        result.foreach {
          response =>
            log.info(s"Photo added:\n${response.entity.asString}")
        }
      }

      case aa: getAlbumsInfo => {
        val result: Future[HttpResponse] = pipeline(Get(Constants.serverURL + "/albums/" + userId))
        result.foreach {
          response =>
            log.info(s"Albums:\n${response.entity.asString}")
        }
      }

    }

    //Image content will be encrypted and server does not know how to decrypt  
    def readImage(name: String): String = {
      var byteArray = Files.readAllBytes(Paths.get(name))
      if (byteArray.length > 0) {
        Base64.encodeBase64String(byteArray)
      } else {
        log.error("No image found at : " + name)
        null
      }
    }
  }
}