

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
    val userAlreadyPresent = "Username already exists"
    val albumCreated = "Album successfully created."
    val albumCreationFailed = "Album creation failed"
    val photoAdded = "Successfully added the photo to user profile"
    val photoAddFailed = "Photo addition to user album failed"
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
case class Photo(userId: String, albumId: String, photoId: String, src: String, message: Option[String] = None, place: Option[String] = None, noStory: Boolean = false)
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
case class getUserAlbums(userId: String)
case class getUserAlbumInfo(userId: String, albumId: String)

class PictureAlbum(val ownerId: String, val albumId: String) {

  var coverPhoto: Option[String] = None
  var createdTime: String
  var description: Option[String] = None
  var place: Option[String] = None
  var updateTime: String
  var photos: ListBuffer[String]

  def populate(pcoverPhoto: Option[String] = None, pdescription: Option[String] = None, pplace: Option[String] = None) {
    coverPhoto = pcoverPhoto
    description = pdescription
    place = pplace
    createdTime = System.currentTimeMillis().toString()
    updateTime = System.currentTimeMillis().toString()
    photos = new ListBuffer[String]
  }

  def addPicture(pic: Picture) {
    if (pic != null) {
      photos.append(pic.photoId)
    }
  }

}

class Picture(val albumId: String, val photoId: String, val src: String) {

  var message: Option[String] = None
  var place: Option[String] = None
  var nostory: Boolean = false

  def populate(pmessage: Option[String] = None, pplace: Option[String] = None, pnostory: Boolean = false) {
    nostory = pnostory
    message = pmessage
    place = pplace
  }
}
