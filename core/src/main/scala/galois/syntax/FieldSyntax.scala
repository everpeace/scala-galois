package galois.syntax

import galois.Field

case class FieldOps[E:Field](e: E)(implicit f:Field[E]) extends Ops[E]{
  def isOne = f.one == e

  def isZero = f.zero == e

  def <+>(x: E) = f.add(e, x)

  def a_inv = f.a_inv(e)

  def <->(x: E) = f.sub(e, x)

  def <*>(x: E) = f.mul(e, x)

  def m_inv = f.m_inv(e)

  def </>(x: E) = f.div(e, x)

  def <**>(i: BigInt) = f.pow(e, i)
}

trait FieldSyntax {
  implicit def toFieldOps[E:Field](e:E):FieldOps[E] = FieldOps(e)

  // unary operators
  def a_inv[E:Field](e:E) = e.a_inv
  def m_inv[E:Field](e:E) = e.m_inv

  // accessor for contextual unit elements
  def zero[E:Field]= implicitly[Field[E]].zero
  def one[E:Field]= implicitly[Field[E]].one
}
