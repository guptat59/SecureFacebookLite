import java.security._
import java.util.concurrent.ConcurrentHashMap
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

object Security {

  var publicKeys = new ConcurrentHashMap[String, PublicKey]()

  var ALGORITHM = "RSA"
  var keyGen = KeyPairGenerator.getInstance(ALGORITHM)
  keyGen.initialize(1024)


  def main(args: Array[String]) {
/*
    var k = generateKey("user1")
    var originalText = "The Far Eastern Party uisahfsdhfasdhg"
    System.out.println("Original: " + originalText)
    var cipherText = encryptAES(originalText, k.getPublic)
    System.out.println("Encrypted AES Key String: " + cipherText(0).toString())
    System.out.println("Encrypted Text Data String: " + cipherText(1).toString())
    var plainText = decryptAES(cipherText, k.getPrivate)
    System.out.println("Decrypted: " + plainText)
  */}


  def getPublicKey(user: String): PublicKey = {
    var x = publicKeys.get(user)
    println("Requested publickey : " + user + " Found : " + x)
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

  def decryptAES(ar: aesResponse, key: Key): String = {

    var secretkey = decryptRSA(ar.ciphedSecretKey, key)

    var aes = Cipher.getInstance("AES")
    var aeskey = new SecretKeySpec(secretkey.getBytes(Constants.charset), "AES")
    aes.init(Cipher.DECRYPT_MODE, aeskey)

    var normaltext = aes.doFinal(ar.ciphedData.getBytes(Constants.charset))
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
