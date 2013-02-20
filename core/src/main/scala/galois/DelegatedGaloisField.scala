package galois

/**
 * Created with IntelliJ IDEA.
 * User: omura
 * Date: 2013/02/19
 * Time: 19:10
 * To change this template use File | Settings | File Templates.
 */
abstract class DelegatedGaloisField[E,D](override val primitive_polynomial:E) extends GaloisField[E]{
  def delegate:GaloisField[D]
  def e2d:E=>D
  def d2e:D=>E

  /**
   * @return additive inverse of e.
   */
  def a_inv(e: E) = d2e(delegate.a_inv(e2d(e)))


  /**
   * @return multiplicative inverse of e.
   */
  def m_inv(e: E) = d2e(delegate.m_inv(e2d(e)))

  def add(a: E, b: E) = d2e(delegate.add(e2d(a),e2d(b)))

  def mul(a: E, b: E) = d2e(delegate.mul(e2d(a),e2d(b)))

  /**
   * @return polynomial representation of e, such like "x^3+x^2+1".
   */
  def polynomialString(e: E) = delegate.polynomialString(e2d(e))
}
