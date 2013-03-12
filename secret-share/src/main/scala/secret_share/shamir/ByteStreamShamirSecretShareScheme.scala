package secret_share.shamir

import akka.util.duration._
import akka.actor.{Props, Actor}
import akka.dispatch.Await
import akka.pattern.ask
import galois._
import galois.GaloisField8Bytes._
import galois.GaloisField4Bytes._
import galois.GaloisField2Bytes._
import secret_share.ThresholdSecretShareScheme

class ByteStreamShamirSecretShareScheme(override val n: Int, override val k: Int) extends Actor with ThresholdSecretShareScheme {
  type CryptoType = (Stream[Byte], Stream[Byte])
  type PlainType = Stream[Byte]

  type Byte8 = (Byte,Byte,Byte,Byte,Byte,Byte,Byte,Byte)
  type Byte4 = (Byte,Byte,Byte,Byte)
  type Byte2 = (Byte,Byte)

  val field8 = GaloisField8Bytes(long2TupBytes(P64))
  val field4 = GaloisField4Bytes(int2TupBytes(P32))
  val field2 = GaloisField2Bytes(short2TupBytes(P16))
  val field1 = DEFAULT_GF_BYTE

  val gen_coef8 = () => {
    val rands = new Array[Byte](8)
    scala.util.Random.nextBytes(rands)
    (rands(0),rands(1),rands(2),rands(3),rands(4),rands(5),rands(6),rands(7))
  }
  val gen_poly8 = (e:Byte8) => ((0, e) :: (1 to k - 1).toList.map((_, gen_coef8()))).toMap
  val scheme8 = context.actorOf(Props(new ShamirSecretShareScheme[Byte8](n, k, field8, gen_poly8)))

  val gen_coef4: () => Byte4 = () => {
    val rands = new Array[Byte](4)
    scala.util.Random.nextBytes(rands)
    (rands(0),rands(1),rands(2),rands(3))
  }
  val gen_poly4 = (e: Byte4) => ((0, e) :: (1 to k - 1).toList.map((_, gen_coef4()))).toMap
  val scheme4 = context.actorOf(Props(new ShamirSecretShareScheme[Byte4](n, k, field4, gen_poly4)))

  val gen_coef2: () => Byte2 = () => {
    val rands = new Array[Byte](2)
    scala.util.Random.nextBytes(rands)
    (rands(0),rands(1))
  }
  val gen_poly2 = (e: Byte2) => ((0, e) :: (1 to k - 1).toList.map((_, gen_coef2()))).toMap
  val scheme2 = context.actorOf(Props(new ShamirSecretShareScheme[Byte2](n, k, field2, gen_poly2)))

  val gen_coef1: () => Byte = () => {
    val rands = new Array[Byte](1)
    scala.util.Random.nextBytes(rands)
    rands(0)
  }
  val gen_poly1 = (e: Byte) => ((0, e) :: (1 to k - 1).toList.map((_, gen_coef1()))).toMap
  val scheme1 = context.actorOf(Props(new ShamirSecretShareScheme[Byte](n, k, field1, gen_poly1)))


  def encrypt(ps: Stream[Byte]): List[(Stream[Byte], Stream[Byte])] = ps match {
    case p8 #:: p7 #:: p6 #:: p5 #:: p4 #:: p3 #:: p2 #:: p1 #:: _ps =>
      val cryptosF = scheme8 ? Plain((p8, p7, p6, p5, p4, p3, p2, p1))
      val Crypto(cryptos) = Await.result(cryptosF, 100 seconds)
      cryptos.zip(encrypt(_ps)).map(t => {
        val x: Byte8 = t._1._1.asInstanceOf[Byte8]
        val s: Byte8 = t._1._2.asInstanceOf[Byte8]
        ((x._1 #:: x._2 #:: x._3 #:: x._4 #:: x._5 #:: x._6 #:: x._7 #:: x._8 #:: t._2._1)
          ,(s._1 #:: s._2 #:: s._3 #:: s._4 #:: s._5 #:: s._6 #:: s._7 #:: s._8 #:: t._2._2))
      })

    case p4 #:: p3 #:: p2 #:: p1 #:: _ps =>
      val cryptosF = scheme4 ? Plain((p4, p3, p2, p1))
      val Crypto(cryptos) = Await.result(cryptosF, 100 seconds)
      cryptos.zip(encrypt(_ps)).map(t => {
        val x: Byte4 = t._1._1.asInstanceOf[Byte4]
        val s: Byte4 = t._1._2.asInstanceOf[Byte4]
        ((x._1 #:: x._2 #:: x._3 #:: x._4 #:: t._2._1)
          , (s._1 #:: s._2 #:: s._3 #:: s._4 #:: t._2._2))
      })

    case p2 #:: p1 #:: _ps =>
      val cryptosF = scheme2 ? Plain((p2, p1))
      val Crypto(cryptos) = Await.result(cryptosF, 100 seconds)
      cryptos.zip(encrypt(_ps)).map(t => {
        val x: Byte2 = t._1._1.asInstanceOf[(Byte, Byte)]
        val sec: Byte2 = t._1._2.asInstanceOf[(Byte, Byte)]
        ((x._1 #:: x._2 #:: t._2._1), (sec._1 #:: sec._2 #:: t._2._2))
      })

    case p1 #:: _ps =>
      val cryptosF1 = scheme1 ? Plain(p1)
      val Crypto(cryptos1) = Await.result(cryptosF1, 100 seconds)
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
      case p8 #:: p7 #:: p6 #:: p5 #:: p4 #:: p3 #:: p2 #:: p1 #:: ps =>
        val cryptos = shared_secrets.map(t => {
          val x = t._1.take(8).toList
          val y = t._2.take(8).toList
          ((x(0), x(1), x(2), x(3), x(4), x(5), x(6), x(7))
            , (y(0), y(1), y(2), y(3), y(4), y(5), y(6), y(7)))
        })
        val plainF = scheme8 ? Crypto(cryptos)
        val Plain(text: Byte8) = Await.result(plainF, 100 seconds)
        text._1 #:: text._2 #:: text._3 #:: text._4 #:: text._5 #:: text._6 #:: text._7 #:: text._8 #:: decrypt(shared_secrets.map(t => (t._1.drop(8), t._2.drop(8))))

      case p4 #:: p3 #:: p2 #:: p1 #:: ps =>
        val cryptos = shared_secrets.map(t => {
          val x = t._1.take(4).toList
          val y = t._2.take(4).toList
          ((x(0), x(1), x(2), x(3)), (y(0), y(1), y(2), y(3)))
        })
        val plainF = scheme4 ? Crypto(cryptos)
        val Plain(text: (Byte, Byte, Byte, Byte)) = Await.result(plainF, 100 seconds)
        text._1 #:: text._2 #:: text._3 #:: text._4 #:: decrypt(shared_secrets.map(t => (t._1.drop(4), t._2.drop(4))))

      case p2 #:: p1 #:: ps =>
        val cryptos = shared_secrets.map(t => {
          val x = t._1.take(2).toList
          val y = t._2.take(2).toList
          ((x(0), x(1)), (y(0), y(1)))
        })
        val plainF = scheme2 ? Crypto(cryptos)
        val Plain(text: Byte2) = Await.result(plainF, 100 seconds)
        text._1 #:: text._2 #:: decrypt(shared_secrets.map(t => (t._1.drop(2), t._2.drop(2))))

      case p1 #:: ps =>
        val cryptos = shared_secrets.map(t => {
          val x = t._1.take(1).toList
          val y = t._2.take(1).toList
          (x(0), y(0))
        })
        val plainF = scheme1 ? Crypto(cryptos)
        val Plain(text: Byte) = Await.result(plainF, 100 seconds)
        Stream(text) #::: decrypt(shared_secrets.map(t => (t._1.drop(1), t._2.drop(1))))

      case _ => Stream.empty
    }
  }

  def receive = {
    case Plain(s: Stream[Byte]) => sender ! Crypto[Stream[Byte]](encrypt(s).map(t => (t._1.toList.toStream, t._2.toList.toStream)))
    case Crypto(c: List[(Stream[Byte], Stream[Byte])]) => sender ! Plain(decrypt(c).toList.toStream)
  }
}
