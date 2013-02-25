package secret_share.shamir

import akka.actor.Actor
import galois.Field

class Encryptor[E,F[E]<:Field[E]] extends Actor{

  protected def receive = {
    case (plain:E, poly: Polynomial[E,F]) => sender ! ((plain, poly.calc(plain)))
  }

}
