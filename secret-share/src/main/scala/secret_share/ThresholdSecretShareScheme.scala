package secret_share

import akka.actor.Actor


trait ThresholdSecretShareScheme{
  type CryptoType
  type PlainType

  require(n > 0, "n must be positive")
  require(0 < k && k <= n, "k must be (0,k].")

  val n:Int
  val k:Int

  def encrypt(ps:PlainType):List[CryptoType]
  def decrypt(ss:List[CryptoType]):PlainType
}
