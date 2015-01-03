package secret_share.shamir

import akka.actor.Actor
import galois.{Polynomial, Field}

class Encryptor[E:Field] extends Actor{

 def receive = {
    case (plain:E, poly: Polynomial[E]) => sender ! ((plain, poly(plain)))
  }

}
