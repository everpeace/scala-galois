package galois

/**
 * GF(2<sup>8</sup>) represented by Byte
 */
case class GaloisFieldByte(override val primitive_polynomial: Byte) extends GaloisFieldAnyVal[Byte](8,primitive_polynomial) {
  import GaloisFieldAnyVal.BIT
  def zero = 0.toByte
  def one = 1.toByte
  def maxValInLong = Byte.MaxValue.toLong
  def xor = (a:Byte, b:Byte) => (a ^ b).toByte
  def leftShift = (a:Byte, i:Int) => (a << i).toByte
  def bit = (a:Byte, i:Int) => (a.toLong & BIT(i)).toInt
}
