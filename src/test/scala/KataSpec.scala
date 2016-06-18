import Kata._
import collection.mutable.Stack
import org.scalatest._

class KataSpec extends FlatSpec with Matchers {

    "numberToWords" should "return the correct result when given 822" in {
        numberToWords(822) should be ("eight hundred and twenty two")
    }

    "wordsToNumber" should "return the correct result when given eight hundred and twenty two" in {
        wordsToNumber("eight hundred and twenty two") should be (822)
    }
}
