package galois

trait Field[E] {

  def zero:E
  def a_unit = zero
  /**
   * @return additive inverse of e.
   */
  def a_inv(e: E): E

  def one: E
  def m_unit = one
  /**
   * @return multiplicative inverse of e.
   */
  def m_inv(e: E): E

  def add(a: E, b: E): E
  def sub(a: E, b: E): E = add(a, a_inv(b))

  def mul(a: E, b: E): E
  def div(a: E, b: E): E = mul(a, m_inv(b))
  def pow(a: E, p: Long): E = p match{
    case _ if (p < 0) => pow(m_inv(a), -1*p)
    case 0  => m_unit
    case _ if ((p & 1L) == 0L) => pow(mul(a,a),p/2)
    case _ if ((p & 1L) == 1L) => mul(a,pow(a,p-1))
  }
}
