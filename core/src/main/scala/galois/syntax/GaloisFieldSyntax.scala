package galois.syntax

import galois.GaloisField

case class GaloisFieldOps[E:GaloisField](e:E)(implicit f:GaloisField[E]) extends Ops[E]{
  def polynomialString: String = f.polynomialString(e)
}

trait GaloisFieldSyntax{
  implicit def toGaloisFieldOps[E:GaloisField](e:E):GaloisFieldOps[E] = GaloisFieldOps(e)

  def polynomialString[E:GaloisField](e:E) = e.polynomialString
  def primitive_polynomial[E:GaloisField] = implicitly[GaloisField[E]].primitive_polynomial
  def primitivePolynomialString[E](gf:GaloisField[E]) = gf.primitivePolynomialString

}