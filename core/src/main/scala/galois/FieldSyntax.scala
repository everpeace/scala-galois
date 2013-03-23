package galois

trait FieldSyntax[E] {

  def isOne:Boolean
  def isZero:Boolean

  def <+>(x:E):E
  def a_inv:E
  def <->(x:E):E

  def <*>(x:E):E
  def m_inv:E
  def </>(x:E):E
  def <**>(i:BigInt):E
}
