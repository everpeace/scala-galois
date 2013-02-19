package galois

/**
 * GF(2<sup>16</sup>) represented by Int
 */
case class GaloisFieldShort(override val primitive_polynomial: Short) extends GaloisFieldAnyVal[Short](16, primitive_polynomial) {
  import GaloisFieldAnyVal.BIT
  def zero = 0.toShort
  def one = 1.toShort
  def maxValInLong = Short.MaxValue.toLong
  def xor = (a:Short,b:Short) => (a ^ b).toShort
  def leftShift = (a:Short,i:Int) => (a << i).toShort
  def bit = (a:Short,i:Int) => a.toLong & BIT(i)
}
