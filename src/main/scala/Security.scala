import java.nio.file.{Paths, Files}
import java.security._
import java.util.concurrent.ConcurrentHashMap
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import org.apache.commons.codec.binary.Base64
import org.apache.commons.lang3.StringEscapeUtils

import scala.util.Random

object Security {

  var publicKeys = new ConcurrentHashMap[String, PublicKey]()

  var ALGORITHM = "RSA"
  var keyGen = KeyPairGenerator.getInstance(ALGORITHM)
  keyGen.initialize(1024)


  def main(args: Array[String]) {

    var k = generateKey("user1")
    var originalText = "The Far Eastern Party uisahfsdhfasdhg"
    originalText = readImage();
    System.out.println("Original: " + originalText)
    var aesres = encryptAES(originalText, k.getPublic)
    System.out.println("Encrypted AES Key String: " + aesres.ciphedSecretKey)
    System.out.println("Encrypted Text Data String: " + aesres.ciphedData)
    var x =  StringEscapeUtils.escapeJson(aesres.ciphedData)
    var plainText = decryptAES(aesres.ciphedSecretKey, aesres.ciphedData, k.getPrivate)
    System.out.println("Decrypted: " + plainText)

  }

  def readImage(): String = {
    var name = "C:\\Users\\Lavenger\\CodeSpace\\facebooklite\\src\\main\\resources\\Hello.jpg"
    var byteArray = Files.readAllBytes(Paths.get(name))
    if (byteArray.length > 0) {
      var x = Base64.encodeBase64(byteArray)
      new String(x, Constants.charset)
    } else {
      null
    }
  }


  def getPublicKey(user: String): Key = {
    var x = publicKeys.get(user)
    //println("Requested publickey : " + user + " Found : " + x)
    x
  }

  def generateKey(user: String): KeyPair = {
    if (!publicKeys.contains(user)) {
      var key = keyGen.generateKeyPair()
      var publickey = key.getPublic()
      publicKeys.put(user, publickey)
      return key
    } else {
      null
    }
  }

  case class aesResponse(secretKey: String, ciphedSecretKey: String, ciphedData: String)

  def encryptAES(text: String, key: Key): aesResponse = {
    var random = new SecureRandom()

    var secretKey: Array[Byte] = Array.fill[Byte](16)(0)
    random.nextBytes(secretKey)

    var aeskey = new SecretKeySpec(secretKey, "AES")

    var aes = Cipher.getInstance("AES")
    aes.init(Cipher.ENCRYPT_MODE, aeskey)

    var ciphedKeyString = encryptRSA(new String(secretKey, Constants.charset), key)
    var ciphedData = aes.doFinal(text.getBytes())

    var ciphedDataString = new String(ciphedData, Constants.charset)

    aesResponse(new String(secretKey, Constants.charset), ciphedKeyString, ciphedDataString)
  }

  def encryptProfileAES(text: String, key: Key, secretKeyString: String): String = {
    var secretKey = secretKeyString.getBytes(Constants.charset)
    var aeskey = new SecretKeySpec(secretKey, "AES")
    var aes = Cipher.getInstance("AES")
    aes.init(Cipher.ENCRYPT_MODE, aeskey)
    var ciphedKeyString = encryptRSA(new String(secretKey, Constants.charset), key)
    var ciphedData = aes.doFinal(text.getBytes())
    var ciphedDataString = new String(ciphedData, Constants.charset)
    StringEscapeUtils.escapeJson(ciphedDataString)
  }

  def decryptAES(ciphedSecretKey: String, ciphedData: String, key: Key): String = {

    var secretkey = decryptRSA(ciphedSecretKey, key)

    var aes = Cipher.getInstance("AES")
    var aeskey = new SecretKeySpec(secretkey.getBytes(Constants.charset), "AES")
    aes.init(Cipher.DECRYPT_MODE, aeskey)

    var normaltext = aes.doFinal(ciphedData.getBytes(Constants.charset))
    new String(normaltext)
  }

  def encryptRSA(text: String, user: Key): String = {
    var cipher = Cipher.getInstance(ALGORITHM)
    var key = user
    cipher.init(Cipher.ENCRYPT_MODE, key)
    var cipherText = cipher.doFinal(text.getBytes(Constants.charset))
    return (new String(cipherText, Constants.charset))
  }

  def decryptRSA(text: String, user: Key): String = {

    var cipher = Cipher.getInstance(ALGORITHM)
    var key = user
    // decrypt the text using the private key
    cipher.init(Cipher.DECRYPT_MODE, key)
    var dectyptedText = cipher.doFinal(text.getBytes(Constants.charset))

    return new String(dectyptedText, Constants.charset)
  }

}
