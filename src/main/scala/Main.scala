import Kata._

object Main {
    def main(args: Array[String]) {

        val ns =
            List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) ++
            (11 to 20) ++
            (21 to 40) ++
            (94 to 127) ++
            List(822) ++
            List(1327) ++
            List(12345) ++
            List(1012345) ++
            List(1000045)

        for (n <- ns) {
            println(s"$n: '${numberToWords(n)}'")
        }

        val wss = List(
            "one",
            "two",
            "three",
            "four",
            "five",
            "six",
            "seven",
            "eight",
            "nine",
            "ten",
            "eleven",
            "twelve",
            "thirteen",
            "fourteen",
            "fifteen",
            "sixteen",
            "seventeen",
            "eighteen",
            "nineteen",
            "twenty",
            "twenty one",
            "thirty four",
            "ninety seven",
            "one hundred and seventy six",
            "eight hundred and twenty two",
            "two hundred")

        for (ws <- wss) {
            println(s"'$ws': ${wordsToNumber(ws)}")
        }
    }
}
