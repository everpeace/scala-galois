import galois.GaloisField16Bytes._
import galois.GaloisField2Bytes._
import galois.GaloisField4Bytes._
import galois.GaloisField8Bytes._

package object galois {
  // Default GF(2^m) and those irreducible polynomials over GF(2).
  object defaults {
    object primitive_polynomials {
      /** ｘ<sup>8</sup> + ｘ<sup>4</sup> + ｘ<sup>3</sup> + ｘ<sup>2</sup> + 1 */
      val P8: Byte = ((1 << 4) | (1 << 3) | (1 << 2) | 1).toByte
      /** ｘ<sup>16</sup> + ｘ<sup>12</sup> + ｘ<sup>3</sup> + ｘ + 1 */
      val P16: Short = ((1 << 12) | (1 << 3) | (1 << 1) | 1).toShort
      /** x<sup>32</sup> + x<sup>22</sup> + ｘ<sup>2</sup> + ｘ + 1 */
      val P32: Int = ((1 << 22) | (1 << 2) | (1 << 1) | 1)
      /** x<sup>64</sup> + x<sup>4</sup> + x<sup>3</sup> + x + 1 */
      val P64: Long = ((1 << 4) | (1 << 3) | (1 << 1) | (1)).toLong
      /** ｘ<sup>128</sup> + ｘ<sup>8</sup> + ｘ<sup>6</sup> + ｘ<sup>5</sup> + ｘ<sup>4</sup> + ｘ +1 */
      val P128 = (1 << 8) | (1 << 6) | (1 << 5) | (1 << 4) | (1 << 1) | 1
    }

    import primitive_polynomials._
    /** default Galois Field instance on Byte (GF(2<sup>8</sup>), represented in 8 bits) */
    val GF_BYTE = GaloisFieldByte(P8)
    /** default Galois Field instance on Short (GF(2<sup>16</sup>), represented in 16 bits) */
    val GF_SHORT = GaloisFieldShort(P16)
    /** default Galois Field instance on Int (GF(2<sup>32</sup>), represented in 32 bits) */
    val GF_INT = GaloisFieldInt(P32)
    /** default Galois Field instance on Long (GF(2<sup>64</sup>), represented in 64 bits) */
    val GF_LONG = GaloisFieldLong(P64)

    /** default Galois Field instance on (Byte, Byte) (GF(2<sup>16</sup>), represented in 2 Bytes (16 bits)) */
    val GF_2_BYTES  = GaloisField2Bytes(short2TupBytes(P16))
    /** default Galois Field instance on (Byte, Byte, Byte, Byte) (GF(2<sup>32</sup>), represented in 4 Bytes (32 bits)) */
    val GF_4_BYTES  = GaloisField4Bytes(int2TupBytes(P32))
    /** default Galois Field instance on (Byte, Byte, ...) (GF(2<sup>64</sup>), represented in 8 Bytes (64 bits) */
    val GF_8_BYTES  = GaloisField8Bytes(long2TupBytes(P64))
    /** default Galois Field instance on (Byte, Byte, ...) (GF(2<sup>128</sup>), represented in 16 Bytes (128 bits)) */
    val GF_16_BYTES = GaloisField16Bytes(byteArray2TupBytes(BigInt(P128).toByteArray))
  }

  // unpimp
  implicit def unpimp[E,F[E]<:Field[E]](p:PimpedFieldSyntax[E,F]):E = p.e
  // repimp GF to F
  implicit def repimpGF2F[E, F[E]<:GaloisField[E]](gfp:PimpedFieldSyntax[E,F]) = FieldPimped(gfp.e,gfp.f)
  // repimp F to GF
  implicit def repimpF2GF[E, F[E]<:GaloisField[E]](p:PimpedFieldSyntax[E,F]) = GaloisFieldPimped(p.e,p.f)

  // useful accessor for pimped values
  def a_inv[E,F[E]<:Field[E]](p:PimpedFieldSyntax[E,F]) = p.a_inv
  def m_inv[E,F[E]<:Field[E]](p:PimpedFieldSyntax[E,F]) = p.m_inv
  def polynomialString[E, F[E]<:GaloisField[E]](p:GaloisFieldPimped[E,F]) = p.polynomialString
  def primitivePolynomialString[E](gf:GaloisField[E]) = gf.primitivePolynomialString
  def primitivePolynomialString[E, F[E]<:GaloisField[E]](p: GaloisFieldPimped[E,F]) = p.f.primitivePolynomialString

  // GF2
  object GF2 extends GaloisField[GF2Element]{
    override def degree = 1
    override def primitive_polynomial = One
    def zero = Zero
    def a_inv(e: GF2Element) = e
    def one = One
    def m_inv(e: GF2Element) = {
      require(e != zero,"inverse of zero doesn't exist.")
      One
    }
    def add(a: GF2Element, b: GF2Element) = (a,b) match{
      case (Zero, Zero)=> Zero
      case (Zero, One) => One
      case (One, Zero) => One
      case (One, One) => Zero
    }
    def mul(a: GF2Element, b: GF2Element) = (a,b) match{
      case (Zero, Zero)=> Zero
      case (Zero, One) => Zero
      case (One, Zero) => Zero
      case (One, One) => One
    }
    def polynomialString(e: GF2Element) = e match{
      case Zero => "0"
      case One => "1"
    }
  }

  // elements of GF2 which is equivalent with Boolean.
  sealed trait GF2Element
  case object One extends GF2Element
  case object Zero extends GF2Element
  implicit def GF2E2Bool(e: GF2Element):Boolean = e match {
    case One => true
    case Zero => false
  }
  implicit def Bool2GF2(b:Boolean):GF2Element = b match{
    case true => One
    case false => Zero
  }

}
