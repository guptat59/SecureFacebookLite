

import spray.http.DateTime
import spray.httpx.SprayJsonSupport
import spray.json._
import java.util.UUID
import scala.collection.mutable.ListBuffer
import java.util.ArrayList

object Constants {
  object privacy {
    val friends = "friends"
    val personal = "personal"
  }

  object messages {
    val success = "Successfully completed"
    val noUser = "User does not exist!!"
    val noPermission = "You dont have permission to perform this operation"
    val alreadyPresent = "Username already exists"
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
case class NewUsersReq(count: Int, prefix: String, suffixLength: Option[Int] = None)
case class NewUserReq(username: String)
case class FriendReq(username: String)
case class UsersList(userIds: Array[String])
case class UserPost(message: String, link: Option[String] = None, place: Option[String] = None, privacy: String, object_attachment: Option[String] = None)
case class PostAdded(uuid: String, message: String)
case class UserPage(posts: Array[UserPost])
case class Album(userId: String, albumId: String, coverPhoto: Option[String] = None, createdTime: Option[String] = None, description: Option[String] = None, place: Option[String] = None, updateTime: Option[String] = None, var photos: Array[String])
case class Photo(userId: String, albumId: String, photoId: String, src: String, message: Option[String] = None, place: Option[String] = None, noStory: Option[Boolean] = None)
case class Success(reason: String)
case class Error(reason: String)

object jsonProtocol extends DefaultJsonProtocol with SprayJsonSupport with NullOptions {
  implicit val NewUsersReqFormat = jsonFormat3(NewUsersReq)
  implicit val NewUserReqFormat = jsonFormat1(NewUserReq)
  implicit val FriendReqFormat = jsonFormat1(FriendReq)
  implicit object NewUsersResFormat extends RootJsonFormat[UsersList] {
    def read(value: JsValue) = UsersList(value.convertTo[Array[String]])
    def write(f: UsersList) = f.userIds.toJson
  }
  implicit val UserFormat = jsonFormat5(User)
  implicit val postFormat = jsonFormat5(UserPost)
  implicit val postAddedFormat = jsonFormat2(PostAdded)
  implicit object postList extends RootJsonFormat[UserPage] {
    def read(value: JsValue) = UserPage(value.convertTo[Array[UserPost]])
    def write(f: UserPage) = f.posts.toJson
  }
  implicit def userPageFormat[Post: JsonFormat] = jsonFormat1(UserPage.apply)
  implicit val albumFormat = jsonFormat8(Album)
  implicit val photoFormat = jsonFormat7(Photo)
  implicit val errorFormat = jsonFormat1(Error)
  implicit val successFormat = jsonFormat1(Success)
}

case class findProfile(userId: String)
case class addPost(postId: String, userId: String, post: UserPost) //extends Seal
case class addFriend(userId: String)
case class requestFriend(userId: String, req: FriendReq)
case class getUserPage(userId: String)
case class getFriendsList(userId: String, frndId: String)
case class getPhotos(userId: String)
case class addPhoto(userId: String, albumId: String, photoId: String)
case class getAlbums(userId: String)
case class addAlbums(userId: String, albumId: String)