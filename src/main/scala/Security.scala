
import java.security.KeyPairGenerator
import java.security.PrivateKey
import java.security.PublicKey
import javax.crypto.Cipher
import java.util.concurrent.ConcurrentHashMap
import scala.collection._
import scala.collection.convert.decorateAsScala._

object Security {
  
  var ALGORITHM = "RSA";
  var publickey : PublicKey = null;
  var privatekey : PrivateKey = null;
  var PublicKeys: concurrent.Map[String, PublicKey] = new ConcurrentHashMap().asScala
  var PrivateKeys: concurrent.Map[String, PrivateKey] = new ConcurrentHashMap().asScala
/* def main(args: Array[String]) {
      generateKey() 
      var originalText = "Text to be encrypted ";
      System.out.println("Original: " + originalText);
      var cipherText = encrypt(originalText, publickey);
      System.out.println("Encrypted: " +cipherText.toString());
      var plainText = decrypt(cipherText, privatekey);
      System.out.println("Decrypted: " + plainText);

  }
*/
  def generateKey(user:String) {
      var keyGen = KeyPairGenerator.getInstance(ALGORITHM);
      keyGen.initialize(1024);
      var key = keyGen.generateKeyPair();
      publickey =  key.getPublic();
      privatekey = key.getPrivate();
      PublicKeys.put(user, publickey)
      PrivateKeys.put(user, privatekey)
  }
 
  def encrypt( text: String,  user:String) : Array[Byte]= {
      var cipher = Cipher.getInstance(ALGORITHM);
      var key =  PublicKeys.getOrElse(user, null)
      cipher.init(Cipher.ENCRYPT_MODE, key);
      var cipherText = cipher.doFinal(text.getBytes());
      return (cipherText)
    }

  def decrypt( text: Array[Byte],  key:PrivateKey):String = {
    
      var cipher = Cipher.getInstance(ALGORITHM);
      // decrypt the text using the private key
      cipher.init(Cipher.DECRYPT_MODE, key);
      var dectyptedText = cipher.doFinal(text);

    return new String(dectyptedText);
  }
  }

     
