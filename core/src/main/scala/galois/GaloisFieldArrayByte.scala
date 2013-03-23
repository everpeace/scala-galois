package galois

case class GaloisFieldArrayByte(override val degree: Int, override val primitive_polynomial: Array[Byte]) extends GaloisFieldAnyVal[Array[Byte]](degree, primitive_polynomial) {
  import GaloisFieldAnyVal.BIT
  require(degree % 8 == 0 && degree >= 8, "degree must be power of 2 and no less than 8. degree=" + degree)

  def zero = {
    val z = new Array[Byte](len)
    for (i<-0 until z.length){
      z(i)=0.toByte
    }
    z
  }

  def one = {
    val z = new Array[Byte](len)
    for (i<-0 until z.length-1){
      z(i) = 0.toByte
    }
    z(z.length-1) = 1.toByte
    z
  }

  // 2^(degree-1)-1
  def maxValInBigInt = {
    val a = new Array[Byte](len)
    (len-1 to 1 by -1).foreach(a(_) = (0x00ff).toByte)
    a(0) = 0x007f.toByte
    BigInt(a)
  }

  def xor = (a: Array[Byte], b: Array[Byte]) => a.zip(b).map(t => ((t._1 ^ t._2)&0x00ff).toByte)

  def leftShiftOneBit = (a: Array[Byte]) =>
    a.zipWithIndex.foldRight((new Array[Byte](len), 0x0000.toByte))((a_i: (Byte, Int), b_c: (Array[Byte], Byte)) => {
      val input_carry = b_c._2
      //assign shifted value
      b_c._1(a_i._2) = ((a_i._1 << 1).toByte | input_carry).toByte
      val output_carry = ((a_i._1 >> 7) & 0x0001).toByte
      (b_c._1, output_carry)
    })._1

  def bit = (a: Array[Byte], i: Int) => {
    val q = i / 8
    val r = i % 8
    a(a.length - 1 - q).toLong & BIT(r)
  }

  private[this] def len = degree / 8
}
