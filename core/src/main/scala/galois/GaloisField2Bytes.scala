package galois


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