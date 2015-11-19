

import spray.http.DateTime
import spray.httpx.SprayJsonSupport
import spray.json._
import java.util.UUID

object Constants {
  object privacy {
    val friends = "friends"
    val personal = "personal"
  }

  object messages {
    val success = "Successfully completed"
    val noUser = "User does not exist!!"
  }
}

object Gender extends Enumeration {
  type Gender = Value
  val Male, Female = Value
}

sealed trait Seal

case class UserLogin(username: String, password: String)
object UserLoginJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val PortofolioFormats = jsonFormat2(UserLogin)
}

//JSON Types
trait anyjson
case class User(userId: String, firstName: String, lastName: String, age: Int, gender: String) extends anyjson()
case class NewUsersReq(count: Int, prefix: String, suffixLength: Int)
case class NewUsersRes(userIds: Array[String])
case class UserPost(message: String, link: String, place: String, privacy: String, object_attachment: String)
case class PostAdded(uuid: String, message: String)
case class UserPage(posts: Array[UserPost])
case class FriendList(list: Array[String])
case class Error(reason: String)

object jsonProtocol extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val NewUsersReqFormat = jsonFormat3(NewUsersReq)
  implicit object NewUsersResFormat extends RootJsonFormat[NewUsersRes] {
    def read(value: JsValue) = NewUsersRes(value.convertTo[Array[String]])
    def write(f: NewUsersRes) = f.userIds.toJson
  }
  implicit val UserFormat = jsonFormat5(User)
  implicit val postFormat = jsonFormat5(UserPost)
  implicit val postAddedFormat = jsonFormat2(PostAdded)
  implicit object postList extends RootJsonFormat[UserPage] {
    def read(value: JsValue) = UserPage(value.convertTo[Array[UserPost]])
    def write(f: UserPage) = f.posts.toJson
  }
  implicit def userPageFormat[Post: JsonFormat] = jsonFormat1(UserPage.apply)
  implicit def friendListFormat = jsonFormat1(FriendList)
  implicit val errorFormat = jsonFormat1(Error)

}

case class findProfile(userId: String)
case class addPost(postId: String, userId: String, post: UserPost) //extends Seal
case class addFriend(userId: String)
case class getUserPage(userId: String)
case class getFriendsList(userId: String)
case class getPhotos(userId: String)
case class getAlbums(userId: String)
