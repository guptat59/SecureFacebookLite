import java.nio.file.{Files, Paths}
import java.security.{KeyPairGenerator, PrivateKey, PublicKey}
import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import javax.crypto.Cipher

import Constants.Privacy
import akka.actor.ActorSelection.toScala
import akka.actor.{Actor, ActorLogging, ActorSystem, Cancellable, Props, actorRef2Scala}
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.apache.commons.codec.binary.Base64
import spray.client.pipelining.sendReceive
import spray.http.ContentType.apply
import spray.http.{HttpResponse, _}
import spray.httpx.RequestBuilding._
import spray.httpx.SprayJsonSupport
import spray.json.AdditionalFormats

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

object FacebookSimulator {

  var namingPrefix = "akka://" + "FBClients" + "/user/"
  val actorConf =
    """
  akka {
    #log-config-on-start = on
    stdout-loglevel = "DEBUG"
    loglevel = "DEBUG"    
  }
    """
  val system = ActorSystem("FBClients", ConfigFactory.parseString(actorConf))
  implicit val timeout = Timeout(1)

  val userPrefix = "user"
  val totalUsers = Constants.totalUsers
  val activeUsers = ((2 * totalUsers) / 3)
  val posts = ((3 * activeUsers) / 5)
  val newuser = if (totalUsers * 0.000005 < 1) 1 else totalUsers * 0.000005
  val scPostTime = ((60 * 60) / posts) + 0.1

  val scAlbumTime = 10 * scPostTime
  val scPhotoTime = 2 * scPostTime
  val scUpdateTime = 5

  val scNewUser = 60 / newuser
  val scFrndReq = 60
  val scView = 0.01
  val postUserPer = (25 * activeUsers) / 100
  val postPhotoPer = (40 * activeUsers) / 100
  val viewPer = totalUsers
  var ALGORITHM = "RSA"

  var keyGen = KeyPairGenerator.getInstance(ALGORITHM)
  keyGen.initialize(1024)

  def main(args: Array[String]) {
    //bootSystem()
    // checkSanity()
    // startSchedulers()

    test()
  }

  def bootSystem(): Unit = {
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

    createAlbums()

    while (albumsAdded.get() < Constants.totalUsers) {
      Thread.sleep(1000)
      println("Waiting till all the users have at least one album!! : " + albumsAdded.get())
    }

    createPhotos()

    while (photosAdded.get() < Constants.totalUsers) {
      Thread.sleep(1000)
      println("Waiting till all the users have at least one album!! : " + albumsAdded.get())
    }
  }

  def checkSanity(): Unit = {
    var firstTime = true

    var userName = userPrefix + 4
    var frndName = userPrefix + 7
    val sleepdelay = 1000

    // Sanity Check
    while (true) {
      Thread.sleep(5000)
      println("##########################################################################################################")
      var user = system.actorSelection(namingPrefix + userName)
      var frnd = system.actorSelection(namingPrefix + frndName)

      if (firstTime) {
        user ? FriendRequest(userName, frndName)
        firstTime = false
      }

      println("UserPost >> ")

      //user ? UserPost(userName, "post by " + userName, Option("google1"), Option("Paris"), Privacy.Friends, Some("uuid")) Thread.sleep(sleepdelay)
      //frnd ? UserPost(frndName, "post by " + frndName, Option("google2"), Option("London"), Privacy.Friends, Some("uuid")) Thread.sleep(sleepdelay)

      println("get profile >> ")

      user ? getProfile(frndName)
      Thread.sleep(sleepdelay)
      frnd ? getProfile(userName)
      Thread.sleep(sleepdelay)

      println("update profile >>")
      var u = new User(userName, "First-" + userName + "u", "Last-" + userName + "u", Random.nextInt(100) + 1, Gender.apply(Random.nextInt(Gender.maxId)).toString(), Relation.Single.toString())
      user ? updateProfile(u)
      Thread.sleep(sleepdelay)
      var f = new User(frndName, "First-" + frndName + "u", "Last-" + frndName + "u", Random.nextInt(100) + 1, Gender.apply(Random.nextInt(Gender.maxId)).toString(), Relation.Single.toString())
      frnd ? updateProfile(f)
      Thread.sleep(sleepdelay)

      println("get profile >> ")

      user ? getProfile(frndName)
      Thread.sleep(sleepdelay)
      frnd ? getProfile(userName)
      Thread.sleep(sleepdelay)

      println("get page >> ")

      user ? getPage()
      Thread.sleep(sleepdelay)
      frnd ? getPage()
      Thread.sleep(sleepdelay)

      println("get friends list>> ")

      user ? getFriendsList(userName, frndName)
      Thread.sleep(sleepdelay)
      frnd ? getFriendsList(frndName, userName)
      Thread.sleep(sleepdelay)

      println("get album info>> ")

      user ? getUserAlbums(userName, userName)
      Thread.sleep(sleepdelay)
      frnd ? getUserAlbums(frndName, frndName)
      Thread.sleep(sleepdelay)

      println("get frnd album info>> ")

      user ? getUserAlbums(userName, frndName)
      Thread.sleep(sleepdelay)
      frnd ? getUserAlbums(frndName, userName)
      Thread.sleep(sleepdelay)

      println("addDynamicAlbumAndPhoto>> ")

      user ? addDynamicAlbumAndPhoto()

      println("addPhotoToExistingAlbum>> ")
      user ? addPhotoToExistingAlbum()

      println("get frnd album info>> ")

      var a = new Album(userName, userName + "-defaultalbum", None, Some(System.currentTimeMillis().toString()), Option("initial album"), Option("Hyderabad"), Some(System.currentTimeMillis().toString()), None)
      user ? updateAlbum(a)
      Thread.sleep(sleepdelay)
      //frnd ? updateAlbum(a) Thread.sleep(sleepdelay)

      println("get frnd album info>> ")

      user ? getUserAlbums(userName, frndName)
      Thread.sleep(sleepdelay)
      frnd ? getUserAlbums(frndName, userName)
      Thread.sleep(sleepdelay)

      Thread.sleep(10000)
    }
  }

  def test(): Unit = {
    var userId = userPrefix + 0
    var key = Security.generateKey(userId)
    var publicKey = key.getPublic()
    var privateKey = key.getPrivate()
    var user = system.actorOf(Props(new UserClient(userId, publicKey, privateKey)), userId)
    var u = new User(userId, "First-" + userId, "Last-" + userId, Random.nextInt(100) + 1, Gender.apply(Random.nextInt(Gender.maxId)).toString(), Relation.Single.toString())
    user ! u
  }


  def startSchedulers(): Unit = {

    import system.dispatcher
    var scpost: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1, "seconds"), FiniteDuration.apply(scPostTime.toLong, "seconds"), (new Runnable {
      def run {
        var userName = userPrefix + (Random.nextInt(postUserPer))
        println(userName + " scpost")
        var user = system.actorSelection(namingPrefix + userName)
        //user ! UserPost(userName, Security.encrypt("Post", userName), None, None, Privacy.Friends, None)
      }
    }))

    var sccount = 1

    var scalbum: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1, "seconds"), FiniteDuration.apply(scAlbumTime.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt(postPhotoPer)
        var userName = userPrefix + r
        println(userName + " scalbum")
        var user = system.actorSelection(namingPrefix + userName)
        user ! addDynamicAlbumAndPhoto()
      }
    }))

    var scphoto: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1, "seconds"), FiniteDuration.apply(scPhotoTime.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt(postPhotoPer)
        var userName = userPrefix + r
        println(userName + " scphoto")
        var user = system.actorSelection(namingPrefix + userName)
        user ! addPhotoToExistingAlbum()
      }
    }))

    var scview: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1, "seconds"), FiniteDuration.apply(scView.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt(totalUsers)
        var userName = userPrefix + r
        println(userName + " scview")
        var user = system.actorSelection(namingPrefix + userName)
        if (r % 2 == 0)
          user ! getProfile(userPrefix + Random.nextInt(Constants.totalUsers))
        else
          user ! getPage()
      }
    }))

    var scfrndreq: Cancellable = system.scheduler.schedule(FiniteDuration.apply(5, "seconds"), FiniteDuration.apply(scFrndReq.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt(Constants.totalUsers)
        var userName = userPrefix + r
        println(userName + " scfrndreq")
        var user = system.actorSelection(namingPrefix + userName)
        user ! FriendRequest(userName, userPrefix + (Random.nextInt(Constants.totalUsers)))
      }
    }))

    var scnewuser: Cancellable = system.scheduler.schedule(FiniteDuration.apply(5, "seconds"), FiniteDuration.apply(scNewUser.toLong, "seconds"), (new Runnable {
      def run {
        val usercount = sccount + totalUsers
        var userName = userPrefix + usercount
       // var user = system.actorOf(Props(new UserClient(userName)), userName)
       // println(userName + " scnewuser")
       // var u = new User(userName, "First-" + userName, "Last-" + userName, Random.nextInt(100) + 1, Gender.apply(Random.nextInt(Gender.maxId)).toString(), Relation.Single.toString());
      //  user ! u
      }
    }))

    var scupdate: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1, "seconds"), FiniteDuration.apply(scUpdateTime.toLong, "seconds"), new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt(postPhotoPer)
        var userName = userPrefix + r
        var user = system.actorSelection(namingPrefix + userName)
        if (r % 2 == 0) {
          var a = new Album(userName, userName + "-defaultalbum", None, Some(System.currentTimeMillis().toString()), Option("initial album"), Option("Hyderabad"), Some(System.currentTimeMillis().toString()), None)
          user ? updateAlbum(a);
        }
        else {
          var u = new User(userName, "First-" + userName + "u", "Last-" + userName + "u", Random.nextInt(100) + 1, Gender.apply(Random.nextInt(Gender.maxId)).toString(), Relation.Single.toString());
          user ! updateProfile(u);
        }
      }
    })

    Thread.sleep(1000000)
    scpost.cancel()
    scalbum.cancel()
    scphoto.cancel()
    scview.cancel()
    scnewuser.cancel()
    scfrndreq.cancel()
    scupdate.cancel()
  }


  var createdUsers = new AtomicInteger(0)
  var frndsAdded = new AtomicInteger(0)
  var albumsAdded = new AtomicInteger(0)
  var photosAdded = new AtomicInteger(0)

  def createUsers(): Unit = {

    for (i <- 0 until Constants.totalUsers) {
      var userId = userPrefix + i
      var key = keyGen.generateKeyPair()
      var publicKey = key.getPublic()
      var privateKey = key.getPrivate()
      var user = system.actorOf(Props(new UserClient(userId, publicKey, privateKey)), userId)
      var u = new User(userId, "First-" + userId, "Last-" + userId, Random.nextInt(100) + 1, Gender.apply(Random.nextInt(Gender.maxId)).toString(), Relation.Single.toString())
      user ! u
    }
  }

  def makeFriends(): Unit = {

    println("Per user frnds : " + Constants.numOfFriends)

    for (i <- 0 until Constants.totalUsers) {
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

      var fr = new FriendRequest(userId, fList)
      user ! fr
    }
  }

  def createAlbums(): Unit = {
    for (i <- 0 until Constants.totalUsers) {
      var userId = userPrefix + i
      var user = system.actorSelection(namingPrefix + userId)
      user ! addDefaultAlbum()
    }
  }

  def createPhotos(): Unit = {
    for (i <- 0 until Constants.totalUsers) {
      var userId = userPrefix + i
      var user = system.actorSelection(namingPrefix + userId)
      user ! addDefaultImages()
    }
  }

  sealed trait seal

  case class getPage()
  case class getProfile(userId: String)
  case class updateProfile(u: User)
  case class addDefaultAlbum()
  case class addDynamicAlbumAndPhoto()
  case class addDefaultImages()
  case class addPhotoToExistingAlbum()
  case class addImage()
  case class addAlbum()
  case class updateAlbum(a: Album)
  case class deleteAlbum(albumId: String)

  class UserClient(userId: String, publicKey: PublicKey, privateKey: PrivateKey) extends Actor with SprayJsonSupport with AdditionalFormats with ActorLogging {
    implicit val system = context.system

    import system.dispatcher

    implicit val timeout = Timeout(1000000)

    var pipeline = (sendReceive)

    def authenticate(): Boolean = {
      log.info("Userid : " + userId + " key " + Security.getPublicKey(userId))
      var publicKeyStr = Base64.encodeBase64String(publicKey.getEncoded())
      var response: HttpResponse = Await.result(pipeline(Post(Constants.serverURL + "/auth/request", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "key" : "$publicKeyStr"}"""))),timeout.duration)
      var encryptedContent = response.entity.asString
      log.info("To be decrypted : " + encryptedContent)
      var token = decryptUsingPrivate(encryptedContent)

      response = Await.result(pipeline(Post(Constants.serverURL + "/auth/verify",HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "token" : "$token"}"""))),timeout.duration)
      if (response.entity.asString.toBoolean) {
        var httpCookie = response.headers.collect { case spray.http.HttpHeaders.`Set-Cookie`(hc) => hc }
        pipeline = addHeader(spray.http.HttpHeaders.Cookie(httpCookie)) ~> sendReceive
        println("Added cookie -> " + httpCookie + " to pipeline header!")
        true
      } else {
        println("Failed to authenticate :( " + userId)
        false
      }
    }

    def receive = {

      case u: User => {
        authenticate()

        Security.generateKey(u.userId)
        var userId = u.userId
        var firstName = u.firstName
        var lastName = u.lastName
        var age = u.age
        var gender = u.gender
        var relation = u.relation
        log.debug("Creating user with id " + userId)
        val result: Future[HttpResponse] = pipeline(Put(Constants.serverURL + "/user", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "firstName" : "$firstName" , "lastName" : "$lastName", "age" : $age , "gender" :  "$gender", "relation" :  "$relation"}""")))
        Await.result(result, timeout.duration)
        result.onComplete {
          x => {
            createdUsers.incrementAndGet()
            x.foreach { res => log.debug(res.entity.asString) }
          }
        }
      }

      case uu: updateProfile => {
        var user = uu.u
        var userId = user.userId
        var firstName = user.firstName
        var lastName = user.lastName
        var age = user.age
        var gender = user.gender
        var relation = user.relation
        log.debug("Updating user with id " + userId)
        val result: Future[HttpResponse] = pipeline(Post(Constants.serverURL + "/user", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "firstName" : "$firstName" , "lastName" : "$lastName", "age" : $age , "gender" :  "$gender", "relation" :  "$relation"}""")))
        Await.result(result, timeout.duration)
        result.onComplete {
          x => {
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
          x => {
            frndsAdded.incrementAndGet()
            x.foreach { res => log.debug(res.entity.asString) }
          }
        }
      }

      case ap: UserPost => {
        var message = ap.message
        var link = ap.link
        var object_attachment = ap.object_attachment
        var place = ap.place
        var privacy = ap.privacy
        var postBy = ap.postby

        val result: Future[HttpResponse] = pipeline(Put(Constants.serverURL + "/user/" + userId + "/feed", HttpEntity(MediaTypes.`application/json`, s"""{"postby": "$postBy", "message": "$message", "link": "$link", "place": "$place", "privacy": "$privacy", "object_attachment": "$object_attachment"}""")))
        Await.result(result, timeout.duration)
        result.onComplete {
          x => {
            x.foreach { res => log.debug(res.entity.asString) }
          }
        }
      }

      case gp: getProfile => {
        val result: Future[HttpResponse] = pipeline(Get(Constants.serverURL + "/user" + "?userId=" + gp.userId))
        Await.result(result, timeout.duration)
        result.onComplete {
          x => {
            x.foreach { res => log.debug(res.entity.asString) }
          }
        }
      }

      case gup: getPage => {
        val result: Future[HttpResponse] = pipeline(Get(Constants.serverURL + "/user/" + userId + "/home"))
        Await.result(result, timeout.duration)
        result.onComplete {
          x => {
            x.foreach { res => log.debug(res.entity.asString) }
          }
        }
      }

      case gfl: getFriendsList => {
        var userID = gfl.userId
        var frndId = gfl.frndId
        val result: Future[HttpResponse] = pipeline(Get(Constants.serverURL + "/user/" + userID + "/friendslist/" + frndId))
        Await.result(result, timeout.duration)
        result.onComplete {
          x => {
            x.foreach { res => log.debug(res.entity.asString) }
          }
        }
      }

      case a: addDefaultAlbum => {
        var result = addAlbum(Album(userId, userId + "-defaultalbum", None, Some(System.currentTimeMillis().toString()), Option("initial album"), Option(Constants.places(Random.nextInt(Constants.places.length))), Some(System.currentTimeMillis().toString()), None))
        result.onComplete {
          x => {
            albumsAdded.incrementAndGet()
            x.foreach { res => log.debug(res.entity.asString) }
          }
        }
      }

      case a: addDynamicAlbumAndPhoto => {
        var albumId = userId + UUID.randomUUID()
        var result = addAlbum(Album(userId, albumId, Some(userId + "-defaultphoto"), Some(System.currentTimeMillis().toString()), Option("dynamic album " + albumId), Option(Constants.places(Random.nextInt(Constants.places.length))), Some(System.currentTimeMillis().toString()), None))
        Await.result(result, timeout.duration)
        result.onComplete {
          x => {
            x.foreach { res => log.debug(res.entity.asString) }
            var photoId = userId + UUID.randomUUID()
            var p = Photo(userId, albumId, photoId, readImage(), Some("Dynamic image" + photoId), Option(Constants.places(Random.nextInt(Constants.places.length))), false)
            var r = addImage(p)
            Await.result(r, timeout.duration)
            r.onComplete { y =>
              y.foreach { z => log.debug(z.entity.asString) }
            }
          }
        }
      }

      case a: addDefaultImages =>
        var src = readImage()
        //userId: String, albumId: String, photoId: String, src: String, message: Option[String] = None, place: Option[String] = None, noStory: Boolean = false)
        var p = Photo(userId, userId + "-defaultalbum", userId + "-defaultphoto", src, Some("Default first image"), Option(Constants.places(Random.nextInt(Constants.places.length))), false)
        var result = addImage(p)
        result.onComplete {
          x => {
            photosAdded.incrementAndGet()
            x.foreach { res => log.debug(res.entity.asString) }
          }
        }

      case p: addPhotoToExistingAlbum => {

        var albums = getUserAlbums(userId)
        //var albumId = albums(Random.nextInt(albums.length)).albumId
        var albumId = userId + "-defaultalbum"
        var photoId = userId + UUID.randomUUID()
        var p = Photo(userId, albumId, photoId, readImage(), Some("Dynamic image to existing album " + photoId), Option(Constants.places(Random.nextInt(Constants.places.length))), false)
        var result = addImage(p)
        Await.result(result, timeout.duration)
        result.onComplete { x =>
          x.foreach {
            response =>
              log.debug(s"Photo added :\n${response.entity.asString}")
          }
        }
      }

      case a: getUserAlbums => {
        getUserAlbums(a.frndId)
      }

      case da: deleteAlbum => {
        val result: Future[HttpResponse] = pipeline(Delete(Constants.serverURL + "/user/" + userId + "/albums", "?albumId=" + da.albumId))
        Await.result(result, timeout.duration)
        result.onComplete {
          x => {
            x.foreach { res =>
              log.debug(res.entity.asString)
            }
          }
        }
      }

      case ua: updateAlbum => {
        var a = ua.a
        var userId = a.userId
        var albumId = a.albumId
        var coverPhoto = if (a.coverPhoto.isDefined) a.coverPhoto.get else ""
        var createdTime = if (a.createdTime.isDefined) a.createdTime.get else ""
        var description = if (a.description.isDefined) a.description.get else ""
        var place = if (a.place.isDefined) a.place.get else ""
        var updateTime = if (a.updateTime.isDefined) a.updateTime.get else ""
        var result: Future[HttpResponse] = pipeline(Post(Constants.serverURL + "/user/" + userId + "/albums", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "albumId" :"$albumId", "coverPhoto" : "$coverPhoto", "createdTime" : "$createdTime", "description" : "$description", "place":"$place", "updateTime" :"$updateTime"}""")))
        Await.result(result, timeout.duration)
        result.onComplete {
          x => {
            x.foreach { res =>
              log.debug(res.entity.asString)
            }
          }
        }
      }
    }

    var pubEncryptCipher = Cipher.getInstance(ALGORITHM)
    pubEncryptCipher.init(Cipher.ENCRYPT_MODE, publicKey)

    var pubDecryptCipher = Cipher.getInstance(ALGORITHM)
    pubDecryptCipher.init(Cipher.DECRYPT_MODE, publicKey)

    var priEncryptCipher = Cipher.getInstance(ALGORITHM)
    priEncryptCipher.init(Cipher.ENCRYPT_MODE, privateKey)

    var priDecryptCipher = Cipher.getInstance(ALGORITHM)
    priDecryptCipher.init(Cipher.DECRYPT_MODE, privateKey)

    def encryptUsingPublic(text: String): String = {
      var cipherText = pubEncryptCipher.doFinal(text.getBytes())
      new String(cipherText, Constants.charset)
    }

    def encryptUsingPrivate(text: String): String = {
      var cipherText = priEncryptCipher.doFinal(text.getBytes())
      new String(cipherText, Constants.charset)
    }

    def decryptUsingPrivate(text: String): String = {
      var dectyptedText = priDecryptCipher.doFinal(text.getBytes(Constants.charset))
      new String(dectyptedText)
    }

    def decryptUsingPublic(text: String): String = {
      var dectyptedText = pubDecryptCipher.doFinal(text.getBytes(Constants.charset))
      new String(dectyptedText)
    }

    def getUserAlbums(frndId: String): Array[Album] = {
      val result: Future[HttpResponse] = pipeline(Get(Constants.serverURL + "/user/" + userId + "/albums" + "?frndId=" + frndId))
      Await.result(result, timeout.duration)
      result.onComplete {
        x => {
          x.foreach { res =>
            log.debug(res.entity.asString)
          }
        }
      }
      null
    }

    def addAlbum(a: Album): Future[HttpResponse] = {
      var userId = a.userId
      var albumId = a.albumId
      var coverPhoto = if (a.coverPhoto.isDefined) a.coverPhoto.get else ""
      var createdTime = if (a.createdTime.isDefined) a.createdTime.get else ""
      var description = if (a.description.isDefined) a.description.get else ""
      var place = if (a.place.isDefined) a.place.get else ""
      var updateTime = if (a.updateTime.isDefined) a.updateTime.get else ""
      pipeline(Put(Constants.serverURL + "/user/" + userId + "/albums", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "albumId" :"$albumId", "coverPhoto" : "$coverPhoto", "createdTime" : "$createdTime", "description" : "$description", "place":"$place", "updateTime" :"$updateTime"}""")))
    }

    def addImage(p: Photo): Future[HttpResponse] = {
      var userId = p.userId
      var albumId = p.albumId
      var photoId = p.photoId
      var src = p.src
      var noStory = p.noStory
      var message = if (p.message.isDefined) p.message.get else ""
      var place = if (p.place.isDefined) p.place.get else ""

      pipeline(Put(Constants.serverURL + "/user/" + userId + "/albums/photo", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "albumId" : "$albumId", "place": "$place","photoId": "$photoId", "src": "$src", "message": "$message", "noStory": $noStory}""")))
    }

    def readImageA(): Array[Byte] = {
      var name = Constants.images(Random.nextInt(Constants.images.length))
      var byteArray = Files.readAllBytes(Paths.get(name))
      if (byteArray.length > 0) {
        Base64.encodeBase64(byteArray)
      } else {
        log.error("No image found at : " + name)
        null
      }
    }

    //Image content will be encrypted and server does not know how to decrypt  
    def readImage(): String = {
      var name = Constants.images(Random.nextInt(Constants.images.length))
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