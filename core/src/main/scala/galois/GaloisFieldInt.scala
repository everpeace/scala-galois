package galois

/**
 * GF(2<sup>32</sup>) represented by Int
 */
case class GaloisFieldInt(override val primitive_polynomial: Int) extends GaloisFieldAnyVal[Int](32, primitive_polynomial) {
  import GaloisFieldAnyVal.BIT
  def zero = 0
  def one = 1
  def maxValInBigInt = Int.MaxValue.toLong
  def xor = (a: Int, b: Int) => a ^ b
  def leftShiftOneBit = (a: Int) => a << 1
  def bit = (a: Int, i: Int) => a.toLong & BIT(i)
}
