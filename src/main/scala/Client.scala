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

object client {
  
  var numOfUsers = 100
  var numOfActiveUsers = (numOfUsers) * 30/100
  var numOfPassiveUsers = (numOfUsers) * 70/100
  var userList = new java.util.ArrayList[String]
  var activeUserList = new java.util.ArrayList[String]
  var passiveUserList = new java.util.ArrayList[String]
  var namingPrefix = "akka://" + "FBClients" + "/user/"
  
  def main(args: Array[String]){
    if(args.length > 0)
    {
     numOfUsers = args(0).toInt
    }
    //create 100 users
    //add 10 friends to each user
    //20-30% users post/add photo/album
    //70% gets friendslist/page of friends/profile of friends/photos/albums
    //user should request for 
    //FBServer.
    val system = ActorSystem("FBClients")
    //for loop for 100 actors
    println("adding users")
    var userName = "user333"
    var user1 = system.actorOf(Props(new UserClient(userName)),userName)
    var userName1 = "user443"
    var user2 = system.actorOf(Props(new UserClient(userName1)),userName1)
    user1 ! addUser()
   // user2 ! addUser()
    Thread.sleep(100)
    /*user1 ! getProfile(userName)
    user2 ! getProfile(userName1)
    user1 ! addFriend(userName1)
    Thread.sleep(100)
    user1 ! addPost("post","","","friends","")
    Thread.sleep(100)
    user1 ! getProfile(userName1)
    Thread.sleep(100)
    user1 ! getFriendList("user1","user2") //resource not found
    Thread.sleep(100)
    user2 ! getPage()*/
    user1 ! addAlbum(new Album(userName,userName+"album1",None,None,None,None,None,Array("photo1", "photo2", "photo3")))
    user2 ! addAlbum(new Album(userName1,userName1+"album1",None,None,None,None,None,Array("photo1", "photo2", "photo3")))
    user1 ! addPhoto(new Photo(userName,userName+"album1","photo","fb",None,None,true))
    user1 ! getAlbumsInfo()
    //user1 ! getPage()
    /*for(i <- 1 to numOfUsers)
    {
      
    }*/
  }
  sealed trait seal
  case class addUser()
  case class addFriend(userName:String)
  case class addPost(message: String, link: String, place:String, privacy: String, objtoattach: String)
  case class getPage()
  case class getProfile(userName:String)
  case class getFriendList(userName:String,frndName:String)
  case class systemBoot()
  case class addBulkUser(userName:String)
  case class addAlbum(a:Album)
  case class addPhoto(p:Photo)
  case class getAlbumsInfo()
  
  class UserClient(user:String) extends Actor with SprayJsonSupport with AdditionalFormats{
  implicit val system = context.system
  import system.dispatcher
  val pipeline = sendReceive  
    def receive  = {
      case sb: systemBoot => {
        val result: Future[HttpResponse] =  pipeline(Post("http://localhost:8080/systemboot",HttpEntity(MediaTypes.`application/json`,s"""{"username": "$user"}""")))
          result.foreach {
        response =>
          println(s"Created User:\n${response.entity.asString}")
        }
      }

      case abu: addBulkUser => {
        var p = abu.userName
        var count = 1
        var prefix = "hat"
        var suffixLength = 1
        val result: Future[HttpResponse] =  pipeline(Post("http://localhost:8080/createusers",HttpEntity(MediaTypes.`application/json`,s"""{"count": $count, "prefix": "$prefix", "suffixLength": $suffixLength}""")))
          result.foreach {
        response =>
          println(s"Created User:\n${response.entity.asString}")
        }
      }
      case au: addUser => {
        val result: Future[HttpResponse] =  pipeline(Post("http://localhost:8080/createuser",HttpEntity(MediaTypes.`application/json`,s"""{"username": "$user"}""")))
          result.foreach {
        response =>
          println(s"Created User:\n${response.entity.asString}")
        }
      }
      case gp : getProfile => {
        val result: Future[HttpResponse] =  pipeline(Get("http://localhost:8080/profile/"+gp.userName))
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
        val result: Future[HttpResponse] =  pipeline(Post("http://localhost:8080/user/"+user+"/feed",HttpEntity(MediaTypes.`application/json`,s"""{"message": "$message", "link": "$link", "place": "$place", "privacy": "$privacy", "object_attachment": "$attachment"}""")))
        result.foreach {
        response =>
          println(s"Post added:\n${response.entity.asString}")
        }
      }
      case gup: getPage => {
        val result: Future[HttpResponse] =  pipeline(Get("http://localhost:8080/user/"+user+"/home"))
        result.foreach {
        response =>
          println(s"Page :\n${response.entity.asString}")
        }
      }
      case af: addFriend => {
        var frndID = af.userName
        val result: Future[HttpResponse] =  pipeline(Post("http://localhost:8080/user/"+user+"/addfriend",HttpEntity(MediaTypes.`application/json`,s"""{"username": "$frndID"}""")))
        result.foreach {
        response =>
          println(s"Added friend:\n${response.entity.asString}")
        }
      }
      case gfl: getFriendList => {
        var userID = gfl.userName
        var frndId = gfl.frndName
        val result: Future[HttpResponse] =  pipeline(Get("http://localhost:8080/user/"+userID+"/friendslist/"+frndId))
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
        val result: Future[HttpResponse] =  pipeline(Post("http://localhost:8080/user/"+user+"/albums/create",HttpEntity(MediaTypes.`application/json`,s"""{"userId": "$userId", "albumId" :"$albumId", "coverPhoto" : "$coverPhoto", "createdTime" : "$createdTime", "description" : "$description", "place":"$place", "updateTime" :"$updateTime", "photos" : [$photos]}""")))
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
        val result: Future[HttpResponse] =  pipeline(Post("http://localhost:8080/user/"+user+"/albums/photo",HttpEntity(MediaTypes.`application/json`,s"""{"userId": "$userId", "albumId" : "$albumId", "place": "$place","photoId": "$photoId", "src": "$src", "message": "$message", "noStory": $noStory}""")))
          result.foreach {
        response =>
          println(s"Photo added:\n${response.entity.asString}")
        }
      }
      case aa: getAlbumsInfo => {
        val result: Future[HttpResponse] =  pipeline(Get("http://localhost:8080/albums/"+user))
          result.foreach {
        response =>
          println(s"Albums:\n${response.entity.asString}")
        }
      }
  }
  }
}
