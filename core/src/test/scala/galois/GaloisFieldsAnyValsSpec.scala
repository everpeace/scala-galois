package galois

import org.specs2.mutable._
import galois._
import galois.defaults._
import galois.syntax.explicitly._

class GaloisFieldsAnyValsSpec extends Specification {

  val P2_1 = "x+1"
  val P2_8 = "x^8+x^4+x^3+x^2+1"
  val P2_16 = "x^16+x^12+x^3+x+1"
  val P2_32 = "x^32+x^22+x^2+x+1"
  val P2_64 = "x^64+x^4+x^3+x+1"

  val R2_1 = One
  val R2_8 = 29.toByte
  val R2_16 = 4107.toShort
  val R2_32 = 4194311
  val R2_64 = 27L

  val GF2_8 = GF_BYTE
  val GF2_16 = GF_SHORT
  val GF2_32 = GF_INT
  val GF2_64 = GF_LONG

  "Each primitive polynomial strings" should {
    "be " + P2_1 + " (GF2)." in {
      GF2.primitivePolynomialString must equalTo(P2_1)
    }

    "be " + P2_8 + " (GF2_8)." in {
      GF2_8.primitivePolynomialString must equalTo(P2_8)
    }
    "be " + P2_16 + " (GF2_16)." in {
      GF2_16.primitivePolynomialString must equalTo(P2_16)
    }
    "be " + P2_32 + " (GF2_32)." in {
      GF2_32.primitivePolynomialString must equalTo(P2_32)
    }
    "be " + P2_64 + " (GF2_64)." in {
      GF2_64.primitivePolynomialString must equalTo(P2_64)
    }
  }

  "Each primitive_polynomial (which represents a primitive polynomial)" should {
    "be " + R2_1 + " (GF2)." in {
      primitive_polynomial(GF2) must equalTo(one(GF2))
    }
    "be " + R2_8 + " (GF2_8)." in {
      primitive_polynomial(GF2_8) must equalTo(R2_8 on GF2_8)
    }
    "be " + R2_16 + " (GF2_16)." in {
      primitive_polynomial(GF2_16) must equalTo(R2_16 on GF2_16)
    }
    "be " + R2_32 + " (GF2_32)." in {
      primitive_polynomial(GF2_32) must equalTo(R2_32 on GF2_32)
    }
    "be " + R2_64 + " (GF2_64)." in {
      primitive_polynomial(GF2_64) must equalTo(R2_64 on GF2_64)
    }
  }

  "zero's polynomial string " should {
    "be 0" in {
      zero(GF2).polynomialString must equalTo("0")
      zero(GF2_8).polynomialString must equalTo("0")
      zero(GF2_16).polynomialString must equalTo("0")
      zero(GF2_32).polynomialString must equalTo("0")
      zero(GF2_64).polynomialString must equalTo("0")
    }
  }
  "a add a_inv" should {
    "be zero" in {
      (one(GF2) <+> a_inv(one(GF2))) must equalTo(zero(GF2))
      ((123.toByte on GF2_8) <+> (123.toByte on GF2_8).a_inv) must equalTo(zero(GF2_8))
      ((123.toShort on GF2_16) <+> (123.toShort on GF2_16).a_inv) must equalTo(zero(GF2_16))
      ((123 on GF2_32) <+> (123 on GF2_32).a_inv) must equalTo(zero(GF2_32))
      ((123L on GF2_64) <+> (123L on GF2_64).a_inv) must equalTo(zero(GF2_64))
    }
  }

  "a add zero" should {
    "be a" in {
      (primitive_polynomial(GF2) <+> zero(GF2)) must equalTo(primitive_polynomial(GF2))
      (primitive_polynomial(GF2_8) <+> zero(GF2_8)) must equalTo(primitive_polynomial(GF2_8))
      (primitive_polynomial(GF2_16) <+> zero(GF2_16)) must equalTo(primitive_polynomial(GF2_16))
      (primitive_polynomial(GF2_32) <+> zero(GF2_32)) must equalTo(primitive_polynomial(GF2_32))
      (primitive_polynomial(GF2_64) <+> zero(GF2_64)) must equalTo(primitive_polynomial(GF2_64))
    }
  }

  "addition" should {
    "be commutative" in {
      (primitive_polynomial(GF2) <+> one(GF2)) must equalTo(one(GF2) <+> primitive_polynomial(GF2))
      (primitive_polynomial(GF2_8) <+> one(GF2_8)) must equalTo(one(GF2_8) <+> primitive_polynomial(GF2_8))
      (primitive_polynomial(GF2_16) <+> one(GF2_16)) must equalTo(one(GF2_16) <+> primitive_polynomial(GF2_16))
      (primitive_polynomial(GF2_32) <+> one(GF2_32)) must equalTo(one(GF2_32) <+> primitive_polynomial(GF2_32))
      (primitive_polynomial(GF2_64) <+> one(GF2_64)) must equalTo(one(GF2_64) <+> primitive_polynomial(GF2_64))
    }
  }

  "a times zero" should {
    "be zero" in {
      (primitive_polynomial(GF2) <*> zero(GF2)) must equalTo(zero(GF2))
      (primitive_polynomial(GF2_8) <*> zero(GF2_8)) must equalTo(zero(GF2_8))
      (primitive_polynomial(GF2_16) <*> zero(GF2_16)) must equalTo(zero(GF2_16))
      (primitive_polynomial(GF2_32) <*> zero(GF2_32)) must equalTo(zero(GF2_32))
      (primitive_polynomial(GF2_64) <*> zero(GF2_64)) must equalTo(zero(GF2_64))
    }
  }

  "a times one" should {
    "be a" in {
      (primitive_polynomial(GF2) <*> one(GF2)) must equalTo(primitive_polynomial(GF2))
      (primitive_polynomial(GF2_8) <*> one(GF2_8)) must equalTo(primitive_polynomial(GF2_8))
      (primitive_polynomial(GF2_16) <*> one(GF2_16)) must equalTo(primitive_polynomial(GF2_16))
      (primitive_polynomial(GF2_32) <*> one(GF2_32)) must equalTo(primitive_polynomial(GF2_32))
      (primitive_polynomial(GF2_64) <*> one(GF2_64)) must equalTo(primitive_polynomial(GF2_64))
    }
  }

  "multiplication" should {
    "be commutative" in {
      ((123.toByte on GF2_8) <*> (345.toByte on GF2_8)) must equalTo((345.toByte on GF2_8) <*> (123.toByte on GF2_8))
      ((123.toShort on GF2_16) <*> (345.toShort on GF2_16)) must equalTo((345.toShort on GF2_16) <*> (123.toShort on GF2_16))
      ((123 on GF2_32) <*> (345 on GF2_32)) must equalTo((345 on GF2_32) <*> (123 on GF2_32))
      ((123L on GF2_64) <*> (345L on GF2_64)) must equalTo((345L on GF2_64) <*> (123L on GF2_64))
    }
  }

  "a times a_inv" should {
    "be one" in {
      ((123.toByte on GF2_8) <*> m_inv(123.toByte on GF2_8)) must equalTo(one(GF2_8))
      ((123.toShort on GF2_16) <*> m_inv(123.toShort on GF2_16)) must equalTo(one(GF2_16))
      ((123 on GF2_32) <*> m_inv(123 on GF2_32)) must equalTo(one(GF2_32))
      ((123L on GF2_64) <*> m_inv(123L on GF2_64)) must equalTo(one(GF2_64))
    }
  }

  "inverse of zero" should {
    "occurs IllegalArgumentException." in {
      m_inv(zero(GF2)) must throwA[IllegalArgumentException]
      m_inv(zero(GF2_8)) must throwA[IllegalArgumentException]
      m_inv(zero(GF2_16)) must throwA[IllegalArgumentException]
      m_inv(zero(GF2_32)) must throwA[IllegalArgumentException]
      m_inv(zero(GF2_64)) must throwA[IllegalArgumentException]
    }
  }
}
