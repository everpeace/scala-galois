package galois.syntax

import galois._
import galois.FieldPimped

package object explicitly {
  // pimp Field
  // "e on field" pimped with injecting given field to implicit context.
  implicit def E2GFElementBuilder[E](e: E) = new FElementBuilder[E](e)
  sealed class FElementBuilder[E](val e: E) {
    def on[F[E]<:Field[E]](f:F[E]): PimpedFieldSyntax[E,F] = FieldPimped(e,f)
  }

  // special element accessor with pimp
  def zero[F[E]<:Field[E],E](f:F[E]) = f.zero on f
  def one[F[E]<:Field[E],E](f:F[E]) = f.one on f
  def primitive_polynomial[E,F[E]<:GaloisField[E]](gf: F[E]) = gf.primitive_polynomial on gf
}
