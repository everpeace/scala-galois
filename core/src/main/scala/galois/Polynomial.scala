package galois

/** polynomial over Field */
case class Polynomial[E](coefs :Map[Int,E])(implicit f:Field[E]) extends (E => E){

  def calc(x:E):E = {
    var ac:E = zero(f)
    for( v <- coefs.toList.map{ case (deg, coef) => coef <*> (x <**> deg)} ){
      ac = ac <+> v
    }
    ac
  }

  def apply(x:E) = calc(x)
}
