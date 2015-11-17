

import spray.http.DateTime
import spray.httpx.SprayJsonSupport
import spray.json._



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
case class User(userId: String, firstName: String, lastName: String, age: Int, gender: String)
case class NewUsersReq(count: Int, prefix: String, suffixLength: Int)
case class NewUsersRes(userIds: Array[String])
case class Post(message: String, link: String, place: String, privacy: String, object_attachment: String)
case class UserPage(posts: Array[Post])

object jsonProtocol extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val NewUsersReqFormat = jsonFormat3(NewUsersReq)
  implicit object NewUsersResFormat extends RootJsonFormat[NewUsersRes] {
    def read(value: JsValue) = NewUsersRes(value.convertTo[Array[String]])
    def write(f: NewUsersRes) = f.userIds.toJson
  }
  implicit val UserFormat = jsonFormat5(User)
  implicit val postFormat = jsonFormat5(Post)
  implicit object postList extends RootJsonFormat[UserPage] {
    def read(value: JsValue) = UserPage(value.convertTo[Array[Post]])
    def write(f: UserPage) = f.posts.toJson
  }
  implicit def userPageFormat[Post: JsonFormat] = jsonFormat1(UserPage.apply)
}

case class getUserProfile(userId: String)
case class addFriend(userId: String)
case class getUserPage(username: String)
case class getFriendsList(username: String)
case class getPhotos(username: String)
case class getAlbums(username: String)
