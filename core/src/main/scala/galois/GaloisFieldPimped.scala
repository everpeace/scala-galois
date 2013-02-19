package galois

case class GaloisFieldPimped[E,F[E]<:GaloisField[E]](override val e:E, override val f:F[E])
  extends Pimped[E,F] with GaloisFieldSyntax[E] {

  def polynomialString: String = f.polynomialString(e)

}
