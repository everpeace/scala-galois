package galois

/**
 * GF(2^degee)
 */
trait GaloisField[E] extends Field[E] {
  require(degree >0, "degree of GaloisField must be positive.")

  def degree:Int

  /**
   * primitive polynomial represented by E.
   * The highest degree term is omitted in this representation because coefficient of the highest degree must be one.
   */
  def primitive_polynomial: E

  /**
   * @return polynomial representation of e, such like "x^3+x^2+1".
   */
  def polynomialString(e: E): String

  /**
   * @return primitive polynomial representation of this galois field.
   */
  def primitivePolynomialString
  = if (degree == 1) "x+" + polynomialString(primitive_polynomial)
    else             "x^" + degree + "+" + polynomialString(primitive_polynomial)
}