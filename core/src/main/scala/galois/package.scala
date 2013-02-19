
package object galois {
  // Default GF(2^m) and those irreducible polynomials over GF(2).
  /* x<sup>64</sup> + x<sup>4</sup> + x<sup>3</sup> + x + 1 */
  val P64: Long = ((1 << 4) | (1 << 3) | (1 << 1) | (1)).toLong
  val DEFAULT_GF_LONG = GaloisFieldLong(P64)

  /* x<sup>32</sup> + x<sup>22</sup> + ｘ<sup>2</sup> + ｘ + 1 */
  val P32: Int = ((1 << 22) | (1 << 2) | (1 << 1) | 1)
  val DEFAULT_GF_INT = GaloisFieldInt(P32)

  /* ｘ<sup>16</sup> + ｘ<sup>12</sup> + ｘ<sup>3</sup> + ｘ + 1 */
  val P16: Short = ((1 << 12) | (1 << 3) | (1 << 1) | 1).toShort
  val DEFAULT_GF_SHORT = GaloisFieldShort(P16)

  /* ｘ<sup>8</sup> + ｘ<sup>4</sup> + ｘ<sup>3</sup> + ｘ<sup>2</sup> + 1 */
  val P8: Byte = ((1 << 4) | (1 << 3) | (1 << 2) | 1).toByte
  val DEFAULT_GF_BYTE = GaloisFieldByte(P8)


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
