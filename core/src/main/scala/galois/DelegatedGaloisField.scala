package galois

abstract class DelegatedGaloisField[E,D](override val primitive_polynomial:E) extends GaloisField[E]{
  def delegate:GaloisField[D]
  def e2d:E=>D
  def d2e:D=>E

  /**
   * @return additive inverse of e.
   */
  def a_inv(e: E) = d2e(delegate.a_inv(e2d(e)))


  /**
   * @return multiplicative inverse of e.
   */
  def m_inv(e: E) = d2e(delegate.m_inv(e2d(e)))

  def add(a: E, b: E) = d2e(delegate.add(e2d(a),e2d(b)))

  def mul(a: E, b: E) = d2e(delegate.mul(e2d(a),e2d(b)))

  /**
   * @return polynomial representation of e, such like "x^3+x^2+1".
   */
  def polynomialString(e: E) = delegate.polynomialString(e2d(e))
}

case class GaloisField2Bytes(override val primitive_polynomial:(Byte,Byte)) extends DelegatedGaloisField[(Byte,Byte),Short](primitive_polynomial){
  import GaloisField2Bytes._
  def degree = 16
  def delegate = GaloisFieldShort(e2d(primitive_polynomial))
  def e2d = tupBytes2Short(_)
  def d2e = short2TupBytes(_)
  def zero = d2e(0.toShort)
  def one = d2e(1.toShort)
}

object GaloisField2Bytes{
  def short2TupBytes(l:Short):(Byte,Byte) = ((l >> 8 & 0xff).toByte, (l & 0xff).toByte)

  def tupBytes2Short(t:(Byte,Byte)):Short = {
    var l:Short = 0
    l = (l | ((t._1.toShort & 0xff) << 8)).toShort
    l = (l | ((t._2.toShort & 0xff))).toShort
    l
  }

}


case class GaloisField4Bytes(override val primitive_polynomial:(Byte,Byte,Byte,Byte)) extends DelegatedGaloisField[(Byte,Byte,Byte,Byte),Int](primitive_polynomial){
  import GaloisField4Bytes._
  def degree = 32
  def delegate = GaloisFieldInt(e2d(primitive_polynomial))
  def e2d= tupBytes2Int(_)
  def d2e = int2TupBytes(_)
  def zero = int2TupBytes(0)
  def one = int2TupBytes(1)
}

object GaloisField4Bytes{
  def int2TupBytes(l:Int):(Byte,Byte,Byte,Byte) = ((l >> 24 & 0xff).toByte, (l >> 16 & 0xff).toByte, (l >> 8 & 0xff).toByte, (l & 0xff).toByte)

  def tupBytes2Int(t:(Byte,Byte,Byte,Byte)) = {
    var l:Int = 0
    l = l | ((t._1.toInt & 0xff) << 24)
    l = l | ((t._2.toInt & 0xff) << 16)
    l = l | ((t._3.toInt & 0xff) << 8)
    l = l | ((t._4.toInt & 0xff))
    l
  }

}


case class GaloisField8Bytes(override val primitive_polynomial:(Byte,Byte,Byte,Byte,Byte,Byte,Byte,Byte)) extends DelegatedGaloisField[(Byte,Byte,Byte,Byte,Byte,Byte,Byte,Byte),Long](primitive_polynomial){
  import GaloisField8Bytes._
  def degree = 64
  def e2d = tupBytes2Long(_)
  def d2e = long2TupBytes(_)
  def delegate = GaloisFieldLong(e2d(primitive_polynomial))
  def zero = d2e(0L)
  def one = d2e(1L)
}

object GaloisField8Bytes{
  def long2TupBytes(l:Long):(Byte,Byte,Byte,Byte,Byte,Byte,Byte,Byte) = (
    (l >> 56 & 0xff).toByte, (l >> 48 & 0xff).toByte, (l >> 40 & 0xff).toByte, (l>> 32 & 0xff).toByte,
    (l >> 24 & 0xff).toByte, (l >> 16 & 0xff).toByte, (l >> 8 & 0xff).toByte, (l & 0xff).toByte
    )

  def tupBytes2Long(t:(Byte,Byte,Byte,Byte,Byte,Byte,Byte,Byte)) = {
    var l:Long = 0L
    l = l | ((t._1.toLong & 0xff) << 56)
    l = l | ((t._2.toLong & 0xff) << 48)
    l = l | ((t._3.toLong & 0xff) << 40)
    l = l | ((t._4.toLong & 0xff) << 32)
    l = l | ((t._5.toLong & 0xff) << 24)
    l = l | ((t._6.toLong & 0xff) << 16)
    l = l | ((t._7.toLong & 0xff) << 8)
    l = l | ((t._8.toLong & 0xff))
    l
  }

}


case class GaloisField16Bytes(override val primitive_polynomial: (Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte))
  extends DelegatedGaloisField[(Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte), Array[Byte]](primitive_polynomial) {

  import GaloisField16Bytes._

  def zero = byteArray2TupBytes(delegate.zero)

  def one = byteArray2TupBytes(delegate.one)

  def degree = 128

  def delegate = new GaloisFieldArrayByte(128,tupBytes2ByteArray(primitive_polynomial))

  def e2d = tupBytes2ByteArray(_)

  def d2e = byteArray2TupBytes(_)
}

object GaloisField16Bytes {
  type Bytes16 = (Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte)

  def byteArray2TupBytes(b: Array[Byte]): Bytes16 = {
    val l = scala.math.min(16, b.length)
    val a = Array(0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte)
    for (i<- scala.math.max(0, b.length-16) until b.length){
      a(16-l+i) = b(i)
    }
    (a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9), a(10), a(11), a(12), a(13), a(14), a(15))
  }

  def tupBytes2ByteArray(t: Bytes16) = {
    val a = new Array[Byte](16)
    a(0) = t._1
    a(1) = t._2
    a(2) = t._3
    a(3) = t._4
    a(4) = t._5
    a(5) = t._6
    a(6) = t._7
    a(7) = t._8
    a(8) = t._9
    a(9) = t._10
    a(10) = t._11
    a(11) = t._12
    a(12) = t._13
    a(13) = t._14
    a(14) = t._15
    a(15) = t._16
    a
  }
}
