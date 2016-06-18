import Kata._
import org.scalacheck.Properties
import org.scalacheck.Gen.choose
import org.scalacheck.Prop.forAll

object PropertyTest extends Properties("Kata") {
    val gen = choose(1, 999)
    property("roundTrip") = forAll(gen) { n => wordsToNumber(numberToWords(n)) == n }
}
