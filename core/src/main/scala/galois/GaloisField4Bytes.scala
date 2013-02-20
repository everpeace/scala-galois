package galois


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