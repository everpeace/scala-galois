package secret_share.shamir

import secret_share.ThresholdSecretShareScheme
import galois._
import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.{Future, ExecutionContext, Await}
import akka.pattern.ask
import java.util.concurrent.Executors

class ShamirSecretShareScheme[E](override val n:Int, override val k:Int, val f:GaloisField[E], val gen_poly: E => Map[Int,E])
  extends Actor with ThresholdSecretShareScheme {
  type CryptoType = (E, E)
  type PlainType = E

  implicit val _f = f

  val one2n:List[E] = (1 to n).toList.map({i =>
    f.primitive_polynomial <**> (i+2L)
  })

  val encryptors = (1 to n).toList.map(_ => context.actorOf(Props(new Actor{
    def receive = {
      case (plain:E, poly: Polynomial[E,GaloisField]) => sender ! ((plain, poly.calc(plain)))
    }
  })))

  def receive = {
    case Plain(v:E) => sender ! Crypto[E](encrypt(v))
    case Crypto(cryptos:List[(E,E)]) => sender ! Plain(decrypt(cryptos))
  }


  def encrypt(p: E):List[(E,E)] = {
    val pool = Executors.newCachedThreadPool()
    implicit val ec = ExecutionContext.fromExecutorService(pool)
    val poly = gen_poly(p)
    val cryptosF = Future.sequence(encryptors.zip(one2n).map(t =>
                                    t._1 ? (t._2, Polynomial[E,GaloisField](poly,f))
                                  ))
    val cryptos = Await.result(cryptosF, 100 seconds)
//    println("input: "+p +"  crypto: "+cryptos)
    ec.shutdown()
    cryptos.asInstanceOf[List[(E,E)]]
  }

  def decrypt(ss: List[(E,E)]):E = {
    var ac = zero(f)
    // \sum_{j\in 0..k-1}{ y_k * l_j }
    for( yl <-ss.zipWithIndex.map(s_i => s_i._1._2 <*> l_constant(s_i._2,ss))){
      ac = ac <+> yl
    }
    ac
  }

  // constant term of Lagrange basis polynomial
  def l_constant(j:Int, ss:List[(E,E)]):E ={
    var ac = one(f)
    // \prod_{m!=j, m\in 0..k-1}{ x_m/(x_j-x_m) }
    for ( m <- 0 until ss.length if m != j){
      ac = ac <*> ( zero(f) <-> (ss(m)._1 </> (ss(j)._1 <-> ss(m)._1)))
    }
    ac
  }
}
