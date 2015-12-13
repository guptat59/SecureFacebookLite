
import java.security.KeyPairGenerator
import java.security.PrivateKey
import java.security.PublicKey
import javax.crypto.Cipher
import java.util.concurrent.ConcurrentHashMap
import scala.collection._
import scala.collection.convert.decorateAsScala._
import java.nio.file.Files
import java.nio.file.Paths
import org.apache.commons.codec.binary.Base64
import java.security.SecureRandom
import javax.crypto.spec.SecretKeySpec
import java.util.UUID

object Security {
  
  var ALGORITHM = "RSA";
  var publickey : PublicKey = null;
  var privatekey : PrivateKey = null;
  var PublicKeys: concurrent.Map[String, PublicKey] = new ConcurrentHashMap().asScala
  var PrivateKeys: concurrent.Map[String, PrivateKey] = new ConcurrentHashMap().asScala
  
  def main(args: Array[String]) {

      generateKey("user1") 
      var originalText = readImage();
     // originalText = "The Far Eastern Party ";
      System.out.println("Original: " + originalText);
      var cipherText = encryptImage(originalText, "user1");
      System.out.println("Encrypted AES Key String: " +cipherText(0).toString());
      System.out.println("Encrypted Text Data String: " +cipherText(1).toString());
      var plainText = decryptImage(cipherText, "user1");
      System.out.println("Decrypted: " + plainText);
  }
  
   def generateKey(user:String) {
      var keyGen = KeyPairGenerator.getInstance(ALGORITHM);
      keyGen.initialize(1024);
      var key = keyGen.generateKeyPair();
      publickey =  key.getPublic();
      privatekey = key.getPrivate();
      PublicKeys.put(user, publickey)
      PrivateKeys.put(user, privatekey)
  }
 
  def encryptImage(text:String,user:String) : Array[String] = {
    
    var random = new SecureRandom();

        var secretKey : Array[Byte] = Array.fill[Byte](16)(0);
        //secretKey = UUID.randomUUID().toString().getBytes()
        random.nextBytes(secretKey)

        var aes = Cipher.getInstance("AES");
        var aeskey = new SecretKeySpec(secretKey, "AES");
        aes.init(Cipher.ENCRYPT_MODE, aeskey);

        var ciphedKey = encrypt(secretKey,user);
        var ciphedData = aes.doFinal(text.getBytes());
  
        var cipherKeyString = new String(ciphedKey, "ISO-8859-1");
        var cipherDataString = new String(ciphedData, "ISO-8859-1");
      
        var arr = Array(cipherKeyString,cipherDataString)
    return arr
  }
  
   def decryptImage(text:Array[String],user:String) : String = {
    
    var random = new SecureRandom();

        var ciphedKey = text(0).getBytes("ISO-8859-1");
        var secretkey = decrypt(ciphedKey, user) 

        var aes = Cipher.getInstance("AES");
        var aeskey = new SecretKeySpec(secretkey,"AES");
        aes.init(Cipher.DECRYPT_MODE, aeskey);

        var normaltext = aes.doFinal(text(1).getBytes("ISO-8859-1"))
    return new String(normaltext)
  }
   
  def encrypt( text: Array[Byte],  user:String) : Array[Byte]= {
      var cipher = Cipher.getInstance(ALGORITHM);
      var key =  PublicKeys.getOrElse(user, null)
      cipher.init(Cipher.ENCRYPT_MODE, key);
      var cipherText = cipher.doFinal(text);
      return (cipherText)
    }

  def decrypt( text: Array[Byte],  user:String):Array[Byte] = {
    
      var cipher = Cipher.getInstance(ALGORITHM);
      var key =  PrivateKeys.getOrElse(user, null)
      // decrypt the text using the private key
      cipher.init(Cipher.DECRYPT_MODE, key);
      var dectyptedText = cipher.doFinal(text);

    return dectyptedText;
  }
  
  def readImage(): String = {
     // var name = Constants.images(Random.nextInt(Constants.images.length))
    var name = "C:/Users/rakeshdrk/facebooklite/src/main/resources/Hello.jpg"
      var byteArray = Files.readAllBytes(Paths.get(name))
      if (byteArray.length > 0) {
        Base64.encodeBase64String(byteArray)
      } else {
        //log.error("No image found at : " + name)
        println("Nothing found")
        null
      }
    }
  }

     
