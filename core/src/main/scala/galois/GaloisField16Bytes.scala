package galois

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
