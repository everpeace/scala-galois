package galois

case class FieldPimped[E,F[E]<:Field[E]](override val e:E, override val f:F[E]) extends PimpedFieldSyntax[E,F]