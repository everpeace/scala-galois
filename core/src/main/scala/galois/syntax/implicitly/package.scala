package galois.syntax

import Predef.{implicitly => implicitVal, _}
import galois._
import galois.FieldPimped

package object implicitly {
  // FIXME this doesn't work...
  // implicit def pimpF[E, F[E]<:Field[E]](e:E)(implicit f:F[E]):PimpedFieldSyntax[E,F] = FieldPimped(e,f)

  implicit def pimpF[E:Field](e:E) = FieldPimped(e,implicitVal[Field[E]])
  implicit def pimpGF[E:GaloisField](e:E) = GaloisFieldPimped(e,implicitVal[GaloisField[E]])

  // special element accessor
  def zero[E:Field]= implicitVal[Field[E]].zero
  def one[E:Field]= implicitVal[Field[E]].one
  def primitive_polynomial[E:GaloisField] = implicitVal[GaloisField[E]].primitive_polynomial
}
