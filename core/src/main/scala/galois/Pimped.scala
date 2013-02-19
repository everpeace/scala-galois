package galois

trait Pimped[E,F[E] <: Field[E]]{
  def e:E
  def f:F[E]
}
