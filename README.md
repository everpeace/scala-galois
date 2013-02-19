scala-galois
====
Galois Field Arithmetic Library in Scala.

examples
----
This library supports both implicit and explicit syntax:

implicit syntax example:
    
     > // step 1. import galois and syntax choice.
     > import galois._, galois.syntax.implicitly._
     > 
     > // step 2. construct field.
     > // sample primitive polynomial over GF(2): x32 + x22 + x2 + x + 1
     > val primitive_poly:Int = ((1 << 22) | (1 << 2) | (1 << 1) | 1)
     > val field = GaloisFieldInt(primitive_poly)
     > 
     > // step 3. inject a field to inplicit.
     > implicit val implicit_field = field
     >
     > // step 4. calculate.
     > 7 <*> 3               // === 9 (which means (x^2+x+1)(x+1) = (x^3+1))
     > 3547.polynomialString // === x^11+x^10+x^8+x^7+1
     
explicit syntax example:

     > // step 1. import galois and syntax choice.
     > import galois._, galois.syntax.explicitly._
     > 
     > // step 2. construct field.
     > // sample primitive polynomial over GF(2): x32 + x22 + x2 + x + 1
     > val primitive_poly:Int = ((1 << 22) | (1 << 2) | (1 << 1) | 1)
     > val field = GaloisFieldInt(primitive_poly)
     >
     > // step 3. calculate.
     > (7 on f) <*> (3 on f)        // === 9 (which means (x^2+x+1)(x+1) = (x^3+1))
     > (3547 on f).polynomialString // === x^11+x^10+x^8+x^7+1 