package secret_share.shamir

import galois._

case class Polynomial[E, F[E] <: Field[E]](coefs :Map[Int,E], f:F[E]) {

  def calc(x:E):E = {
    implicit val _f = f
    var ac:E = zero(f)
    for( v <- coefs.toList.map{ case (deg, coef) => coef <*> (x <**> deg)} ){
      ac = ac <+> v
    }
    ac
  }
}
