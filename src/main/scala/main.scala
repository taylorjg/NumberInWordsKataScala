import Kata._

object Main {
    def main(args: Array[String]) {
        val n1 = 822
        val ws = numberToWords(n1)
        val n2 = wordsToNumber(ws)
        println(s"numberToWords($n1): $ws")
        println(s"wordsToNumber($ws): $n2")
    }
}
