package secret_share.shamir

trait Msg
case class Plain[P](text:P) extends Msg
case class Crypto[S](crypto:List[(S,S)]) extends Msg