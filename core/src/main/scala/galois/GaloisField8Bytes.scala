package galois


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