import scala.concurrent.Future
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import spray.client.pipelining.Get
import spray.client.pipelining.Post
import spray.client.pipelining.sendReceive
import spray.client.pipelining.sendReceive$default$3
import spray.http.ContentType.apply
import spray.http.HttpEntity
import spray.http.HttpResponse
import spray.http.MediaTypes
import spray.httpx.SprayJsonSupport
import spray.json.AdditionalFormats
import java.util.Arrays.ArrayList
import akka.actor.ActorRef
import com.sun.xml.internal.ws.policy.privateutil.PolicyUtils.Commons
import scala.util.Random
import scala.concurrent.duration.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import akka.actor.Cancellable
import java.util.concurrent.TimeUnit
import scala.collection.mutable.HashMap

object client {

  var namingPrefix = "akka://" + "FBClients" + "/user/"

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
    val system = ActorSystem("FBClients")
    //for loop for 100 actors
    println("adding users")
    var u = "default"
    var u1 = system.actorOf(Props(new UserClient(u)), u)
    u1 ! systemBoot()
    for (i <- 1 to Constants.totalUsers)
    {
      var userName = "user"+i
      var user = system.actorOf(Props(new UserClient(userName)),userName)
    }
    var userName = "user3333"
    var user1 = system.actorOf(Props(new UserClient(userName)), userName)
    var userName1 = "user4444"
    var user2 = system.actorOf(Props(new UserClient(userName1)), userName1)
    user1 ! addUser()
   
    import system.dispatcher
    var scheduler1: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1.toLong, "seconds"), FiniteDuration.apply(.1.toLong, "seconds"), (new Runnable {
      def run {
        var userName = "user" + (Random.nextInt((Constants.totalUsers * 10) / 100) + 1)
        println(userName + " scheduler1")
        var user = system.actorSelection(namingPrefix + userName)
        user ! addPost("post", "", "", "friends", "")
      }
    }))

    var sccount = 1
    var scheduler2: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1, "seconds"), FiniteDuration.apply(.2.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt((Constants.totalUsers * 20) / 100) + 1
        var userName = "user" + r
        println(userName + " scheduler2")
        var user = system.actorSelection(namingPrefix + userName)
        if (r % 2 == 0)
          user ! addAlbum(new Album(userName, userName + "album" + sccount, None, None, None, None, None, None))
        else
          user ! addPhoto(new Photo(userName, userName + "album1", "uphoto" + sccount, "fb", None, None, true))
      }
    }))

    var scheduler3: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1, "seconds"), FiniteDuration.apply(.001.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt((Constants.totalUsers * 80) / 100) + 1 + (Constants.totalUsers * 20) / 100
        var userName = "user" + r
        println(userName + " scheduler3")
        var user = system.actorSelection(namingPrefix + userName)
        if (r % 2 == 0)
          user ! getProfile("user" + Random.nextInt(Constants.totalUsers))
        else
          user ! getPage()
      }
    }))

    var scheduler4: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1, "seconds"), FiniteDuration.apply(5.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt(Constants.totalUsers) + 1
        var userName = "user" + r
        println(userName + " scheduler4")
        var user = system.actorSelection(namingPrefix + userName)
        user ! addFriend("user" + (Random.nextInt(Constants.totalUsers) + 1))
      }
    }))

    scheduler1.cancel()
    scheduler2.cancel()
    scheduler3.cancel()
    scheduler4.cancel()
  }
  sealed trait seal
  case class addUser()
  case class addFriend(userName: String)
  case class addPost(message: String, link: String, place: String, privacy: String, objtoattach: String)
  case class getPage()
  case class getProfile(userName: String)
  case class getFriendList(userName: String, frndName: String)
  case class systemBoot()
  case class addBulkUser(userName: String)
  case class addAlbum(a: Album)
  case class addPhoto(p: Photo)
  case class getAlbumsInfo()

  class UserClient(user: String) extends Actor with SprayJsonSupport with AdditionalFormats {
    implicit val system = context.system
    import system.dispatcher
    val pipeline = sendReceive
    def receive = {
      case sb: systemBoot => {
        val result: Future[HttpResponse] = pipeline(Post("http://localhost:8080/systemboot", HttpEntity(MediaTypes.`application/json`, s"""{"username": "$user"}""")))
        result.foreach {
          response =>
            println(s"System Boot:\n${response.entity.asString}")
        }
      }

      case abu: addBulkUser => {
        var p = abu.userName
        var count = 1
        var prefix = "hat"
        var suffixLength = 1
        val result: Future[HttpResponse] = pipeline(Post("http://localhost:8080/createusers", HttpEntity(MediaTypes.`application/json`, s"""{"count": $count, "prefix": "$prefix", "suffixLength": $suffixLength}""")))
        result.foreach {
          response =>
            println(s"Created User:\n${response.entity.asString}")
        }
      }
      case au: addUser => {
        val result: Future[HttpResponse] = pipeline(Post("http://localhost:8080/createuser", HttpEntity(MediaTypes.`application/json`, s"""{"username": "$user"}""")))
        result.foreach {
          response =>
            println(s"Created User:\n${response.entity.asString}")
        }
      }
      case gp: getProfile => {
        val result: Future[HttpResponse] = pipeline(Get("http://localhost:8080/profile/" + gp.userName))
        result.foreach {
          response =>
            println(s"Profile Page of user:\n${response.entity.asString}")
        }
      }
      case ap: addPost => {
        var privacy = ap.privacy
        var attachment = ap.objtoattach
        var place = ap.place
        var message = ap.message
        var link = ap.link
        val result: Future[HttpResponse] = pipeline(Post("http://localhost:8080/user/" + user + "/feed", HttpEntity(MediaTypes.`application/json`, s"""{"message": "$message", "link": "$link", "place": "$place", "privacy": "$privacy", "object_attachment": "$attachment"}""")))
        result.foreach {
          response =>
            println(s"Post added:\n${response.entity.asString}")
        }
      }
      case gup: getPage => {
        val result: Future[HttpResponse] = pipeline(Get("http://localhost:8080/user/" + user + "/home"))
        result.foreach {
          response =>
            println(s"Page :\n${response.entity.asString}")
        }
      }
      case af: addFriend => {
        var frndID = af.userName
        val result: Future[HttpResponse] = pipeline(Post("http://localhost:8080/user/" + user + "/addfriend", HttpEntity(MediaTypes.`application/json`, s"""{"username": "$frndID"}""")))
        result.foreach {
          response =>
            println(s"Added friend:\n${response.entity.asString}")
        }
      }
      case gfl: getFriendList => {
        var userID = gfl.userName
        var frndId = gfl.frndName
        val result: Future[HttpResponse] = pipeline(Get("http://localhost:8080/user/" + userID + "/friendslist/" + frndId))
        result.foreach {
          response =>
            println(s"Friends List:\n${response.entity.asString}")
        }
      }
      case aa: addAlbum => {
        var userId = aa.a.userId
        var albumId = aa.a.albumId
        var coverPhoto = aa.a.coverPhoto
        var createdTime = aa.a.createdTime
        var description = aa.a.description
        var place = aa.a.place
        var updateTime = aa.a.updateTime
        var photos = aa.a.photos
        println(photos.mkString(","))
        var photostring = aa.a.photos.mkString(",")
        val result: Future[HttpResponse] = pipeline(Post("http://localhost:8080/user/" + user + "/albums/create", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "albumId" :"$albumId", "coverPhoto" : "$coverPhoto", "createdTime" : "$createdTime", "description" : "$description", "place":"$place", "updateTime" :"$updateTime", "photos" : [""]}""")))

        result.foreach {
          response =>
            println(s"added album:\n${response.entity.asString}")
        }
      }
      case aa: addPhoto => {
        var userId = aa.p.userId
        var albumId = aa.p.albumId
        var message = aa.p.message
        var noStory = aa.p.noStory
        var photoId = aa.p.photoId
        var place = aa.p.place
        var src = aa.p.src
        val result: Future[HttpResponse] = pipeline(Post("http://localhost:8080/user/" + user + "/albums/photo", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "albumId" : "$albumId", "place": "$place","photoId": "$photoId", "src": "$src", "message": "$message", "noStory": $noStory}""")))
        result.foreach {
          response =>
            println(s"Photo added:\n${response.entity.asString}")
        }
      }
      case aa: getAlbumsInfo => {
        val result: Future[HttpResponse] = pipeline(Get("http://localhost:8080/albums/" + user))
        result.foreach {
          response =>
            println(s"Albums:\n${response.entity.asString}")
        }
      }
    }
  }
}
