package secret_share.shamir

import secret_share.ThresholdSecretShareScheme
import galois._
import galois.syntax.implicitly._
import akka.actor._
import akka.util.duration._
import akka.pattern.ask
import akka.dispatch.{ExecutionContext, Await, Future}
import java.util.concurrent.Executors

class ShamirSecretShareScheme[E](override val n:Int, override val k:Int, val f:GaloisField[E], val gen_poly: E => Map[Int,E])
  extends Actor with ThresholdSecretShareScheme {
  type CryptoType = (E, E)
  type PlainType = E

  implicit val _f = f

  val one2n:List[E] = (1 to n).toList.map({i =>
    implicit val _f = f
    unpimp(primitive_polynomial <**> (i+2L))
  })

  val encryptors = (1 to n).toList.map(_ => context.actorOf(Props(new Actor{
    protected def receive = {
      case (plain:E, poly: Polynomial[E,GaloisField]) => sender ! ((plain, poly.calc(plain)))
    }
  })))

  protected def receive = {
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

  // FIXME unpimp をどうなくす?
  def decrypt(ss: List[(E,E)]):E = {
    var ac = zero(f)
    // \sum_{j\in 0..k-1}{ y_k * l_j }
    for( yl <-ss.zipWithIndex.map(s_i => s_i._1._2 <*> l_constant(s_i._2,ss))){
      ac = unpimp(ac <+> yl)
    }
    ac
  }

  // constant term of Lagrange basis polynomial
  // FIXME unpimp をどうなくす?
  def l_constant(j:Int, ss:List[(E,E)]):E ={
    var ac = one(f)
    // \prod_{m!=j, m\in 0..k-1}{ x_m/(x_j-x_m) }
    for ( m <- 0 until ss.length if m != j){
      ac = unpimp(ac <*> ( zero(f) <-> (ss(m)._1 </> (ss(j)._1 <-> ss(m)._1))))
    }
    ac
  }
}
