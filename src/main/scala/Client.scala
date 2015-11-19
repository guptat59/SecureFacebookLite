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
    val system = ActorSystem("FBClients")
    //for loop for 100 actors
    var userName = "user1"
    var user = system.actorOf(Props(new UserClient(userName)),userName)
    println("adding user")
    user ! addUser()
    Thread.sleep(10000)
    println("adding post")
    //var node = system.actorSelection(Constants.namingPrefix + hashName)
    user ! addPost("post","","","friends","")
    
    /*for(i <- 1 to numOfUsers)
    {
      
    }*/
  }
  sealed trait seal
  case class addPost(message: String, link: String, place:String, privacy: String, objtoattach: String)
  case class getPage()
  case class addFriend(userName:String) 
  case class getHomePage()
  case class getFriendList(userName:String,frndName:String)
  case class getProfile(userName:String)
  case class addUser()
  case class addBulkUser(userName:String)
  //case class addPhoto()
  //case class addAlbum()
  //case class getPhotos()
  //page
  //friend list
  //profile
  
  class UserClient(user:String) extends Actor with SprayJsonSupport with AdditionalFormats{
  implicit val system = context.system
  import system.dispatcher
  val pipeline = sendReceive  
    def receive  = {
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
        val result: Future[HttpResponse] =  pipeline(Get("http://localhost:8080/profile/"+user))
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
          println(s"Post response:\n${response.entity.asString}")
        }
      }
      case gup: getPage => {
        val result: Future[HttpResponse] =  pipeline(Get("http://localhost:8080/user/"+user+"/home"))
        result.foreach {
        response =>
          println(s"Post response:\n${response.entity.asString}")
        }
      }
      case af: addFriend => {
        var frndID = af.userName
        val result: Future[HttpResponse] =  pipeline(Post("http://localhost:8080/user/"+user+"/addfriend",HttpEntity(MediaTypes.`application/json`,s"""{"username": "$frndID"}""")))
        result.foreach {
        response =>
          println(s"Post response:\n${response.entity.asString}")
        }
      }
      case gfl: getFriendList => {
        var userID = gfl.userName
        var frndId = gfl.frndName
        val result: Future[HttpResponse] =  pipeline(Post("http://localhost:8080/user/"+userID+"/friendslist/"+frndId))
        result.foreach {
        response =>
          println(s"Post response:\n${response.entity.asString}")
        }
      }
  }
  }
}