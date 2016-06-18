object Kata {

    val unitsToString = Map(
        (0 -> ""),
        (1 -> "one"),
        (2 -> "two"),
        (3 -> "three"),
        (4 -> "four"),
        (5 -> "five"),
        (6 -> "six"),
        (7 -> "seven"),
        (8 -> "eight"),
        (9 -> "nine"))

    val firstDecadeToString = Map(
        (10 -> "ten"),
        (11 -> "eleven"),
        (12 -> "twelve"),
        (13 -> "thirteen"),
        (14 -> "fourteen"),
        (15 -> "fifteen"),
        (16 -> "sixteen"),
        (17 -> "seventeen"),
        (18 -> "eighteen"),
        (19 -> "nineteen"))

    val tensToString = Map(
        (10 -> "ten"),
        (20 -> "twenty"),
        (30 -> "thirty"),
        (40 -> "forty"),
        (50 -> "fifty"),
        (60 -> "sixty"),
        (70 -> "seventy"),
        (80 -> "eighty"),
        (90 -> "ninety"))

    def numberToWords(n: Int): String = {

        if (n < 1) {
            throw new IllegalArgumentException
        }

        if (n < 10) {
            return unitsToString(n) 
        }

        if (n < 20) {
            return firstDecadeToString(n) 
        }

        if (n < 100) {
            val units = n % 10
            val tens = n - units
            val part1 = tensToString(tens) 
            val part2 = unitsToString(units)
            return s"$part1 $part2" 
        }

        if (n < 1000) {
            val hundreds = n / 100
            val remainder = n - (hundreds * 100)
            val part1 = unitsToString(hundreds) + " hundred"
            val part2 = if (remainder > 0) " and " + numberToWords(remainder) else "" 
            return s"$part1$part2" 
        }

        if (n < 10000) {
            val thousands = n / 1000
            val remainder = n - (thousands * 1000)
            val part1 = unitsToString(thousands) + " thousand"
            val part2 = if (remainder > 0) " and " + numberToWords(remainder) else "" 
            return s"$part1$part2" 
        }

        throw new IllegalArgumentException
    }

    def wordsToNumber(ws: String): Int = {
        1
    } 
}
