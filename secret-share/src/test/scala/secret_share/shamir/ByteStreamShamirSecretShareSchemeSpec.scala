package secret_share.shamir

import akka.actor.{Props, ActorSystem}
import akka.pattern.ask
import galois._
import akka.dispatch.Await
import akka.util.duration._
import org.specs2.mutable._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck

class ByteStreamShamirSecretShareSchemeSpec extends Specification with org.specs2.time.NoTimeConversions with ScalaCheck{

  "decrypt encrypted message with length 4k" should {
    "be the same with plain text" in {
        assert_encrypt_and_decrypt("This is a very secret theory")
    }
  }

  "decrypt encrypted message with length 4k+1" should {
    "be the same with plain text" in {
      assert_encrypt_and_decrypt("This is a very secret theory.")
    }
  }

  "decrypt encrypted message with length 4k+2" should {
    "be the same with plain text" in {
      assert_encrypt_and_decrypt("This is a very secret theory..")
    }
  }

  "decrypt encrypted message with length 4k+3" should {
    "be the same with plain text" in {
      assert_encrypt_and_decrypt("This is a very secret theory...")
    }
  }

  "decrypt encrypted message with long string" should {
    "be the same with plain text" in {
      val text =
        """
          |Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi volutpat, urna nec fermentum mollis, nunc felis dapibus lacus, vel laoreet risus dolor nec tortor. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Nunc quis diam at felis rutrum molestie quis sit amet nisl. Praesent sit amet sodales magna. Nulla facilisi. Nulla egestas auctor ligula, vitae congue metus lobortis consequat. Phasellus posuere dictum libero in cursus. Phasellus eleifend mollis nisl sit amet fringilla. Vivamus iaculis, eros eget adipiscing pellentesque, velit augue dignissim sapien, eu sodales leo urna et mi. Pellentesque mattis, justo quis pharetra vehicula, nisl libero consequat erat, quis porttitor lorem dui pharetra odio. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Duis nisl nibh, scelerisque ac scelerisque at, eleifend non risus. Morbi id sapien odio. Ut porta ante nec magna pretium lobortis. Duis at nulla ante, vel tempus nisi.
          |
          |Sed velit diam, fermentum sit amet iaculis eu, imperdiet eu magna. Sed sem nisi, sagittis quis lobortis non, porttitor in nisl. Sed quis odio blandit enim pellentesque volutpat laoreet sodales ligula. Proin felis massa, molestie quis dignissim ac, sagittis at tortor. Nulla pretium iaculis nibh vel imperdiet. Suspendisse congue semper gravida. Fusce eu tortor ante, nec aliquet eros. Cras feugiat accumsan tincidunt. Integer interdum neque convallis dui euismod suscipit. In iaculis pharetra varius. Donec ac augue id felis rutrum pharetra sed nec nibh. Maecenas nec mi eu libero tempor iaculis. Quisque laoreet, velit quis dictum viverra, nulla lectus iaculis ipsum, in cursus ipsum enim id sapien.
          |
          |Pellentesque a urna vehicula eros sodales facilisis quis at justo. Donec interdum nisi ut tellus lobortis aliquam. Cras ullamcorper tristique eleifend. Cras convallis nisi pharetra nunc pretium aliquet. Phasellus imperdiet leo ut nibh pharetra imperdiet. Morbi convallis, nibh et placerat dictum, quam justo accumsan nisl, in gravida mauris eros ac purus. Morbi luctus molestie est, vel ornare mauris tincidunt in. Mauris ac dui sit amet est blandit dapibus. Nam faucibus, augue ut ultricies interdum, sem elit lobortis diam, et elementum ipsum leo nec eros. Vivamus feugiat porta justo ac fermentum.
          |
          |Proin at orci ligula. Phasellus justo orci, rutrum nec scelerisque sit amet, dignissim at elit. Vivamus vestibulum cursus laoreet. Nam nec magna vel lectus auctor venenatis. Quisque vel erat dolor. Nunc arcu lectus, ornare at elementum non, pellentesque ut dui. Nam a facilisis nunc. Phasellus posuere velit sit amet felis viverra non malesuada risus tempor. Nulla facilisi. Duis quis risus metus, vel posuere elit. Sed vestibulum, felis at rhoncus suscipit, sem tortor faucibus ligula, in aliquet dui magna in quam. Cras placerat sem non ante eleifend bibendum. Nullam malesuada semper sagittis. Mauris pretium, orci quis ornare elementum, erat metus tempor risus, in tincidunt arcu nisi ut tortor. Curabitur sagittis congue tellus in faucibus. Vestibulum orci dolor, ultricies et lobortis vel, posuere id odio.
          |
          |In volutpat elementum ante, vitae convallis velit pellentesque at. Nam eleifend felis sed metus mattis interdum. Integer pharetra ante id sem fringilla id aliquet dolor malesuada. Maecenas ut vulputate metus. Proin tempor orci quis massa elementum eu adipiscing tortor rutrum. Cras urna erat, pulvinar sed pharetra id, accumsan ac mi. Suspendisse sit amet arcu justo. Duis euismod lectus id arcu fringilla rutrum.
        """.stripMargin
      assert_encrypt_and_decrypt(text)
    }
  }
  def assert_encrypt_and_decrypt(text:String)={
    val system = ActorSystem("SecretShareLibrary")
    import scala.util.Random
    val r = new Random
    import secret_share.shamir._

    val textBytes = text.getBytes("UTF-8")

//    print("plain text: ")
//    println(textBytes.mkString("::"))
    val actor = system.actorOf(Props(new ByteStreamShamirSecretShareScheme(5,3)))
    val crypto = for {
      Crypto(sec) <- actor ? Plain(Stream(textBytes:_*))
    } yield sec
//    println("encrypted: " + Await.result(crypto, 5 second))

    val decrypt = for {
      c <- crypto
      Plain(p) <- actor ? Crypto(c)
    } yield p
    val decrypted_text = Await.result(decrypt, 100 second)
//    println("decrypted: " + decrypted_text)

    system.shutdown()

    decrypted_text must equalTo(Stream(text:_*))
  }
}
