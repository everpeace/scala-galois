package galois

import org.specs2.mutable._

class GaloisFieldsAnyValsSpec extends Specification {
  implicit val GF2_8 =  defaults.GF_BYTE
  implicit val GF2_16 = defaults.GF_SHORT
  implicit val GF2_32 = defaults.GF_INT
  implicit val GF2_64 = defaults.GF_LONG

  // These are primitive polynomials in string representation.
  val P2_1 = "x+1"
  val P2_8 = "x^8+x^4+x^3+x^2+1"
  val P2_16 = "x^16+x^12+x^3+x+1"
  val P2_32 = "x^32+x^22+x^2+x+1"
  val P2_64 = "x^64+x^4+x^3+x+1"

  // These are elements for primitive polynomials above
  val R2_1 = One
  val R2_8 = 29.toByte
  val R2_16 = 4107.toShort
  val R2_32 = 4194311
  val R2_64 = 27L

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
      primitive_polynomial(GF2_8) must equalTo(R2_8)
    }
    "be " + R2_16 + " (GF2_16)." in {
      primitive_polynomial(GF2_16) must equalTo(R2_16)
    }
    "be " + R2_32 + " (GF2_32)." in {
      primitive_polynomial(GF2_32) must equalTo(R2_32)
    }
    "be " + R2_64 + " (GF2_64)." in {
      primitive_polynomial(GF2_64) must equalTo(R2_64)
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
      // this is on GF2
      (one(GF2) <+> a_inv(one(GF2))) must equalTo(zero(GF2))
      // this is on GF2_8
      ((123.toByte) <+> (123.toByte).a_inv) must equalTo(zero(GF2_8))
      // this is on GF2_16
      ((123.toShort) <+> (123.toShort).a_inv) must equalTo(zero(GF2_16))
      // this is on GF2_32
      ((123) <+> (123).a_inv) must equalTo(zero(GF2_32))
      // this is on GF2_64
      ((123L) <+> (123L).a_inv) must equalTo(zero(GF2_64))
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
      (123.toByte <*> 345.toByte) must equalTo(345.toByte <*> 123.toByte)
      (123.toShort <*> 345.toShort) must equalTo(345.toShort <*> 123.toShort)
      (123 <*> 345) must equalTo(345 <*> 123)
      (123L <*> 345L) must equalTo(345L <*> 123L)
    }
  }

  "a times a_inv" should {
    "be one" in {
      (123.toByte <*> m_inv(123.toByte)) must equalTo(one(GF2_8))
      (123.toShort <*> m_inv(123.toShort)) must equalTo(one(GF2_16))
      (123 <*> m_inv(123)) must equalTo(one(GF2_32))
      (123L <*> m_inv(123L)) must equalTo(one(GF2_64))
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
