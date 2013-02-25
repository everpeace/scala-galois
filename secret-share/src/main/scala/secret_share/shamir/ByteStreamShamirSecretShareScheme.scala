package secret_share.shamir

import akka.util.duration._
import akka.actor.{Props, Actor}
import akka.dispatch.Await
import akka.pattern.ask
import galois._
import galois.GaloisField4Bytes._
import galois.GaloisField2Bytes._
import secret_share.ThresholdSecretShareScheme

class ByteStreamShamirSecretShareScheme(override val n: Int, override val k: Int) extends Actor with ThresholdSecretShareScheme {
  type CryptoType = (Stream[Byte], Stream[Byte])
  type PlainType = Stream[Byte]

  val field4 = GaloisField4Bytes(int2TupBytes(P32))
  val field2 = GaloisField2Bytes(short2TupBytes(P16))
  val field1 = DEFAULT_GF_BYTE

  val gen_coef4: () => (Byte, Byte, Byte, Byte) = () => (1.toByte, 1.toByte, 1.toByte, 1.toByte)
  val gen_poly4 = (e: (Byte, Byte, Byte, Byte)) => ((0, e) :: (1 to k - 1).toList.map((_, gen_coef4()))).toMap
  val scheme4 = context.actorOf(Props(new ShamirSecretShareScheme[(Byte, Byte, Byte, Byte)](n, k, field4, gen_poly4)))

  val gen_coef2: () => (Byte, Byte) = () => (1.toByte, 1.toByte)
  val gen_poly2 = (e: (Byte, Byte)) => ((0, e) :: (1 to k - 1).toList.map((_, gen_coef2()))).toMap
  val scheme2 = context.actorOf(Props(new ShamirSecretShareScheme[(Byte, Byte)](n, k, field2, gen_poly2)))

  val gen_coef1: () => Byte = () => 1.toByte
  val gen_poly1 = (e: Byte) => ((0, e) :: (1 to k - 1).toList.map((_, gen_coef1()))).toMap
  val scheme1 = context.actorOf(Props(new ShamirSecretShareScheme[Byte](n, k, field1, gen_poly1)))


  def encrypt(ps: Stream[Byte]): List[(Stream[Byte], Stream[Byte])] = ps match {
    case p4 #:: p3 #:: p2 #:: p1 #:: _ps =>
      val cryptosF = scheme4 ? Plain((p4, p3, p2, p1))
      val Crypto(cryptos) = Await.result(cryptosF, 5 seconds)
      cryptos.zip(encrypt(_ps)).map(t => {
        val x: (Byte, Byte, Byte, Byte) = t._1._1.asInstanceOf[(Byte, Byte, Byte, Byte)]
        val sec: (Byte, Byte, Byte, Byte) = t._1._2.asInstanceOf[(Byte, Byte, Byte, Byte)]
        ((x._1 #:: x._2 #:: x._3 #:: x._4 #:: t._2._1), (sec._1 #:: sec._2 #:: sec._3 #:: sec._4 #:: t._2._2))
      })

    case Stream(p3, p2, p1) =>
      val cryptosF2 = scheme2 ? Plain((p3, p2))
      val cryptosF1 = scheme1 ? Plain(p1)
      val Crypto(cryptos2) = Await.result(cryptosF2, 5 seconds)
      val Crypto(cryptos1) = Await.result(cryptosF1, 5 seconds)
      (cryptos2.zip(cryptos1)).map(t => {
        val x2: (Byte, Byte) = t._1._1.asInstanceOf[(Byte, Byte)]
        val sec2: (Byte, Byte) = t._1._2.asInstanceOf[(Byte, Byte)]
        val x1: Byte = t._2._1.asInstanceOf[Byte]
        val sec1: Byte = t._2._2.asInstanceOf[Byte]
        ((x2._1 #:: x2._2 #:: x1 #:: Stream.empty[Byte]), (sec2._1 #:: sec2._2 #:: sec1 #:: Stream.empty[Byte]))
      })

    case Stream(p2, p1) =>
      val cryptosF2 = scheme2 ? Plain((p2, p1))
      val Crypto(cryptos2) = Await.result(cryptosF2, 5 seconds)
      cryptos2.map(t => {
        val x: (Byte, Byte) = t._1.asInstanceOf[(Byte, Byte)]
        val sec: (Byte, Byte) = t._2.asInstanceOf[(Byte, Byte)]
        ((x._1 #:: x._2 #:: Stream.empty[Byte]), (sec._1 #:: sec._2 #:: Stream.empty[Byte]))
      })

    case Stream(p1) =>
      val cryptosF1 = scheme1 ? Plain(p1)
      val Crypto(cryptos1) = Await.result(cryptosF1, 5 seconds)
      cryptos1.map(t => {
        val x: Byte = t._1.asInstanceOf[Byte]
        val sec: Byte = t._2.asInstanceOf[Byte]
        ((x #:: Stream.empty[Byte]), (sec #:: Stream.empty[Byte]))
      })

    case _ => (1 to n).toList.map(_ => (Stream.empty, Stream.empty))
  }

  def decrypt(ss: List[(Stream[Byte], Stream[Byte])]): Stream[Byte] = {
    val shared_secrets = ss.take(k)
    shared_secrets(0)._1 match {
      case p4 #:: p3 #:: p2 #:: p1 #:: ps =>
        val cryptos = shared_secrets.map(t => {
          val x = t._1.take(4).toList
          val y = t._2.take(4).toList
          ((x(0), x(1), x(2), x(3)), (y(0), y(1), y(2), y(3)))
        })
        val plainF = scheme4 ? Crypto(cryptos)
        val Plain(text: (Byte, Byte, Byte, Byte)) = Await.result(plainF, 5 seconds)
        text._1 #:: text._2 #:: text._3 #:: text._4 #:: decrypt(shared_secrets.map(t => (t._1.drop(4), t._2.drop(4))))

      case Stream(p3, p2, p1) =>
        val cryptos2 = shared_secrets.map(t => {
          val x = t._1.take(2).toList
          val y = t._2.take(2).toList
          ((x(0), x(1)), (y(0), y(1)))
        })
        val cryptos1 = shared_secrets.map(t => {
          val x = t._1.drop(2).take(1).toList
          val y = t._2.drop(2).take(1).toList
          (x(0), y(0))
        })
        val plainF2 = scheme2 ? Crypto(cryptos2)
        val plainF = scheme1 ? Crypto(cryptos1)
        val Plain(text2: (Byte, Byte)) = Await.result(plainF2, 5 seconds)
        val Plain(text: Byte) = Await.result(plainF, 5 seconds)
        Stream(text2._1, text2._2, text) #::: decrypt(shared_secrets.map(t => (t._1.drop(3), t._2.drop(3))))

      case Stream(p2, p1) =>
        val cryptos = shared_secrets.map(t => {
          val x = t._1.take(2).toList
          val y = t._2.take(2).toList
          ((x(0), x(1)), (y(0), y(1)))
        })
        val plainF = scheme2 ? Crypto(cryptos)
        val Plain(text: (Byte, Byte)) = Await.result(plainF, 5 seconds)
        Stream(text._1, text._2) #::: decrypt(shared_secrets.map(t => (t._1.drop(2), t._2.drop(2))))

      case Stream(p1) =>
        val cryptos = shared_secrets.map(t => {
          val x = t._1.take(1).toList
          val y = t._2.take(1).toList
          (x(0), y(0))
        })
        val plainF = scheme1 ? Crypto(cryptos)
        val Plain(text: Byte) = Await.result(plainF, 5 seconds)
        Stream(text) #::: decrypt(shared_secrets.map(t => (t._1.drop(1), t._2.drop(1))))

      case _ => Stream.empty
    }
  }

  def receive = {
    case Plain(s: Stream[Byte]) => sender ! Crypto[Stream[Byte]](encrypt(s).map(t => (t._1.toList.toStream, t._2.toList.toStream)))
    case Crypto(c: List[(Stream[Byte], Stream[Byte])]) => sender ! Plain(decrypt(c).toList.toStream)
  }
}
