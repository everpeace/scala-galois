package secret_share.shamir

import galois._

case class Polynomial[E, F[E] <: Field[E]](coefs :Map[Int,E], f:F[E]) {
  import galois.syntax.implicitly._

  // FIXME unpimp をどうなくす?
  def calc(x:E):E = {
    implicit val _f = f
    var ac:E = zero(f)
    for( v <- coefs.toList.map{ case (deg, coef) => (coef <*> (x <**> deg))} ){
      ac = unpimp(ac <+> v)
    }
    ac
  }
}
