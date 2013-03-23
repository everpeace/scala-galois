package galois

trait PimpedFieldSyntax[E, F[E] <: Field[E]] extends Pimped[E, F] with FieldSyntax[Pimped[E, F]] {
  import galois.syntax.explicitly._
  def isOne = f.one == e

  def isZero = f.zero == e

  def <+>(x: Pimped[E, F]) = {
    require(x.f == f, "operation must be element on the same field.")
    f.add(e, x.e) on f
  }

  def a_inv = f.a_inv(e) on f

  def <->(x: Pimped[E, F]) = {
    require(x.f == f, "operation must be element on the same field.")
    f.sub(e, x.e) on f
  }

  def <*>(x: Pimped[E, F]) = {
    require(x.f == f, "operation must be element on the same field.")
    f.mul(e, x.e) on f
  }

  def m_inv = f.m_inv(e) on f

  def </>(x: Pimped[E, F]) = {
    require(x.f == f, "operation must be element on the same field.")
    f.div(e, x.e) on f
  }

  def <**>(i: BigInt) = f.pow(e, i) on f
}