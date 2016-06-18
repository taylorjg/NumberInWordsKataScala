import Kata._
import org.scalacheck.Properties
import org.scalacheck.Gen.choose
import org.scalacheck.Prop.{forAll, AnyOperators}

object PropertyTest extends Properties("Kata") {

    val gen = choose(1, 999)

    property("roundTrip") = forAll(gen) { n: Int => {
            val s = numberToWords(n)
            val n2 = wordsToNumber(s) 
            n2 ?= n
        }
    }
}
