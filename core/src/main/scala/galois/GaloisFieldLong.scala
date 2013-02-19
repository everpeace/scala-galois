package galois

/**
 * GF(2<sup>64</sup>) represented by Long
 */
case class GaloisFieldLong(override val primitive_polynomial: Long) extends GaloisFieldAnyVal[Long](64, primitive_polynomial) {
  import GaloisFieldAnyVal.BIT
  def zero = 0L
  def one = 1L
  def maxValInLong = Long.MaxValue
  def xor = (a:Long, b:Long)=> a^b
  def leftShift = (a:Long, i:Int) => a << i
  def bit = (a:Long, i:Int) => a & BIT(i)
}
