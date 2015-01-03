package galois

/**
 * abstract case class for GFs represented by Long, Int, Short, Byte.
 */
abstract class GaloisFieldAnyVal[T](override val degree:Int, override val primitive_polynomial:T)
  extends GaloisField[T] { self:GaloisField[T] =>
  // operations which are specific to T.
  def zero: T
  def one: T
  def maxValInBigInt: BigInt
  def xor: (T, T) => T
  def leftShiftOneBit: T => T
  def bit: (T, Int) => Long


  def a_inv(e: T) = e

  // FIXME: this is too naive: e^(-1) = e^(2^m-2).
  // let l be bit length of T, then T can't represent 2^l-1 because it is signed,
  // so we can compute inverse by e^((2^l-1)*2).
  def m_inv(e: T) = {
      require(e != zero, "inverse of zero doesn't exist.")
      pow(pow(e, maxValInBigInt), 2)
  }

  // addition of GF(2^D) is XOR.
  def add(a: T, b: T): T = xor(a, b)

  /**
   * @return a * b on this field.
   */
  def mul(a: T, b: T): T = (a, b) match {
    case (x, y) if x == zero || b == zero => zero
    case _ => {
      var ret: T = zero;
      for (i <- 0 until degree) {
        // if b includes x^i (x is primitive_polynomial of primitive polynomial.)
        if (bit(b, i) != 0L) {
          // add a * x^i
          ret = add(ret, mul_rootpow(a, i));
        }
      }
      ret
    }
  }

  /**
   * @return a*x on GF(2^^D) (x is primitive primitive_polynomial)
   */
  private[this] def mul_root(a: T): T =
    if ((bit(a,degree - 1)) != 0L) {
      // aにD-1次の項があれば桁あふれするので x^D = P を足す(xorする);
      add(leftShiftOneBit(a), primitive_polynomial)
    } else {
      // 基本的にa倍は左シフトするだけ
      leftShiftOneBit(a)
    }

  /**
   * @return a*x^i on GF(2^D) (x is primitive primitive_polynomial)
   */
  private[this] def mul_rootpow(a: T, i: Int): T = i match {
    case x if x >= degree || x < 0 => throw new UnsupportedOperationException("i must be less than " + degree + "[" + i + "]");
    case 0L => a
    case _ => {
      var ret: T = a;
      for (j <- 1 to i) {
        ret = mul_root(ret);
      }
      ret;
    }
  }

  def polynomialString(e: T) = {
    val ret: StringBuffer = new StringBuffer()
    var isFirst = true
    for (i<- (degree-1) to 0 by -1 ){
      if ((bit(e, i)) != 0L) {
        // 最上位ビットだけ見て0でなかったら
        if (isFirst == false) {
          ret.append("+")
        } else {
          isFirst = false
        }
        // その次数の項を付け加える
        i match {
          case 0 =>
            ret.append("1");
          case 1 =>
            ret.append("x");
          case _ => {
            ret.append("x^");
            ret.append(i);
          }
        }
      }
    }
    val retStr = ret.toString()
    if(retStr =="") "0" else retStr;
  }
}

object GaloisFieldAnyVal{
  /**
   * bit mask for Long
   */
  val BIT: Array[Long] = Array(
    1L <<  0, 1L <<  1, 1L <<  2, 1L <<  3, 1L <<  4, 1L <<  5, 1L <<  6, 1L << 7,
    1L << 8,  1L <<  9, 1L << 10, 1L << 11, 1L << 12, 1L << 13, 1L << 14, 1L << 15,
    1L << 16, 1L << 17, 1L << 18, 1L << 19, 1L << 20, 1L << 21, 1L << 22, 1L << 23,
    1L << 24, 1L << 25, 1L << 26, 1L << 27, 1L << 28, 1L << 29, 1L << 30, 1L << 31,
    1L << 32, 1L << 33, 1L << 34, 1L << 35, 1L << 36, 1L << 37, 1L << 38, 1L << 39,
    1L << 40, 1L << 41, 1L << 42, 1L << 43, 1L << 44, 1L << 45, 1L << 46, 1L << 47,
    1L << 48, 1L << 49, 1L << 50, 1L << 51, 1L << 52, 1L << 53, 1L << 54, 1L << 55,
    1L << 56, 1L << 57, 1L << 58, 1L << 59, 1L << 60, 1L << 61, 1L << 62, 1L << 63);
}

/**
 * GF(2<sup>8</sup>) represented by Byte
 */
case class GaloisFieldByte(override val primitive_polynomial: Byte) extends GaloisFieldAnyVal[Byte](8,primitive_polynomial) {
  import GaloisFieldAnyVal.BIT
  def zero = 0.toByte
  def one = 1.toByte
  def maxValInBigInt = Byte.MaxValue.toLong
  def xor = (a:Byte, b:Byte) => (a ^ b).toByte
  def leftShiftOneBit = (a:Byte) => (a << 1).toByte
  def bit = (a:Byte, i:Int) => a.toLong & BIT(i)
}

/**
 * GF(2<sup>16</sup>) represented by Int
 */
case class GaloisFieldShort(override val primitive_polynomial: Short) extends GaloisFieldAnyVal[Short](16, primitive_polynomial) {
  import GaloisFieldAnyVal.BIT
  def zero = 0.toShort
  def one = 1.toShort
  def maxValInBigInt = Short.MaxValue.toLong
  def xor = (a:Short,b:Short) => (a ^ b).toShort
  def leftShiftOneBit = (a:Short) => (a << 1).toShort
  def bit = (a:Short,i:Int) => a.toLong & BIT(i)
}

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

/**
 * GF(2<sup>64</sup>) represented by Long
 */
case class GaloisFieldLong(override val primitive_polynomial: Long) extends GaloisFieldAnyVal[Long](64, primitive_polynomial) {
  import GaloisFieldAnyVal.BIT
  def zero = 0L
  def one = 1L
  def maxValInBigInt = Long.MaxValue
  def xor = (a:Long, b:Long)=> a^b
  def leftShiftOneBit = (a:Long) => a << 1
  def bit = (a:Long, i:Int) => a & BIT(i)
}

/** GF(2<sup>d</sup>) represented by Array[Byte]*/
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
