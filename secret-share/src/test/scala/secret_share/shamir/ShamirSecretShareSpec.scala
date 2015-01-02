package secret_share.shamir

import scala.concurrent.duration._
import scala.concurrent.Await
import akka.actor.{Props, ActorSystem}
import akka.pattern.ask
import galois._
import org.specs2.mutable._

class ShamirSecretShareSpec extends Specification with org.specs2.time.NoTimeConversions{

  "decrypt encrypted message" should {
    "be the same with plain text" in {
      val system = ActorSystem("SecretShareLibrary")
      import scala.util.Random
      val r = new Random
      import secret_share.shamir._

      val text = 135
//      println("plain text: "+ text)
      val actor = system.actorOf(Props(new ShamirSecretShareScheme[Int](5, 3, DEFAULT_GF_INT,
        (e: Int) => {
          val cs = ((0, e) :: (1 to 2).toList.map((_, r.nextInt))).toMap
//          println("generated polynomial: " + cs)
          cs
        }
      )))
      val crypto = for {
        Crypto(sec) <- actor ? Plain(text)
      } yield sec
//      println("encrypted: " + Await.result(crypto, 5 second))

      val decrypt = for {
        c <- crypto
        Plain(p) <- actor ? Crypto(c)
      } yield p
      val decrypted_text = Await.result(decrypt, 5 second)
//      println("decrypted: " + decrypted_text)

      system.shutdown()

      decrypted_text must equalTo(text)
    }
  }

}
