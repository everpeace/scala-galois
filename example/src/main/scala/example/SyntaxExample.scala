package example

object SyntaxExample extends App{
  // step 1. import galois._
  import galois._

  // step 2. set primitive polynomial and generator GaloisField{Long|Int|Short|Byte}
  //  sample primitive polynomial over GF(2)
  //    x32 + x22 + x2 + x + 1
  //    in binary representation: 0000 0100 0000 0000 0000 0000 0111
  //    in hex representation: 0x0400007
  //    in integer representation: 4194311
  val primitive_poly:Int = ((1 << 22) | (1 << 2) | (1 << 1) | 1)
  val field = GaloisFieldInt(primitive_poly)

  // step 3. inject a field.
  implicit val implicit_field = field

  // step 4. you can do field's calculation in FieldSyntax
  //   <+>: addition,  <->: subtraction,  zero: additive identity, a_inv: additive inversion
  //   <*>: multiplication </>: division,  one: multiplicative identity, m_inv: multiplicative inversion
  // FIXME: better examples are required.
  6 <+> 3      // === 5  (which means (x^2+x)+(x+1) = (x^2+1)
  7 <*> 3      // === 9  (which means (x^2+x+1)(x+1) = (x^3+1)
  3547 </> 3647   // === 1432094972 on f
                  // which means (x^11+x^10+x^8+x^7+x^6+x^4+x^3+x+1)/(x^11+x^10+x^9+x^5+x^4+x^3+x^2+x+1)
                  // (x^11+x^10+x^8+x^7+x^6+x^4+x^3+x+1)*(x^30+x^26+x^25+x^24+x^23+x^20+x^19+x^17+x^15+x^14+x^12+x^7+x^6+x^5+x^4+1)
                  // = x^30+x^28+x^26+x^24+x^22+x^20+x^19+x^18+x^11+x^7+x^6+x^5+x^4+x^3+x^2
  3547.polynomialString // === x^11+x^10+x^8+x^7+1
  4194311.m_inv    // === 114973017 on f
                   // which means (x^22+x^2+x+1)(x^31+x^27+x^25+x^19+x^18+x^17+x^16+x^13+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+1) = 1
}
