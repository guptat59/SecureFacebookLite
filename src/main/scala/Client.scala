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

object client {
  
  var numOfUsers = 100
  var numOfActiveUsers = (numOfUsers) * 30/100
  var numOfPassiveUsers = (numOfUsers) * 70/100
  
  var userList = new java.util.ArrayList[UserClient]
  var activeUserList = new java.util.ArrayList[UserClient]
  var passiveUserList = new java.util.ArrayList[UserClient]
  
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
    val system = ActorSystem("abc")
    var user = system.actorOf(Props(new UserClient("1")),"1")
    user ! addUser("1")
    Thread.sleep(10000)
    user ! getProfile("howl1")
    /*for(i <- 1 to numOfUsers)
    {
      
    }*/
  }
  sealed trait seal
  case class addPost(post: String)
  case class addFriend(userName:String) 
  case class getHomePage()
  case class getFriendList(userName:String)
  case class getProfile(userName:String)
  case class addUser(userName:String)
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
      case au: addUser => {
        var us = user.toString()
        var p = au.userName
        var count = 5
        var prefix = "howl"
        var suffixLength = 1
        println("almost there")
        val result: Future[HttpResponse] =  pipeline(Post("http://localhost:8080/createusers",HttpEntity(MediaTypes.`application/json`,s"""{"count": $count, "prefix": "$prefix", "suffixLength": $suffixLength}""")))
          result.foreach {
        response =>
          println(s"HomePage Response:\n${response.entity.asString}")
        }
      }
      case gp : getProfile => {
        var p = gp.userName
        val result: Future[HttpResponse] =  pipeline(Get("http://localhost:8080/profile/"+p))
          result.foreach {
        response =>
          println(s"HomePage Response:\n${response.entity.asString}")
        }
      }
      case ap: addPost => {
        var us = user.toString()
        var p = ap.post
        pipeline(Post("http://localhost:8080/post",HttpEntity(MediaTypes.`application/json`,s"""{"message": "$p", "link": "abc", "place": "abc", "privacy": "abc", "object_attachment": "abc"}""")))        
      }
    }
  }
}