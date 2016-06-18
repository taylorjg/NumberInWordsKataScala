object Kata {

    private val unitsToString = Map(
        (0 -> "zero"),
        (1 -> "one"),
        (2 -> "two"),
        (3 -> "three"),
        (4 -> "four"),
        (5 -> "five"),
        (6 -> "six"),
        (7 -> "seven"),
        (8 -> "eight"),
        (9 -> "nine"))

    private val firstDecadeToString = Map(
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

    private val tensToString = Map(
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

        def helper(d: Int, s: String): String = {
            val sep = " and "
            val n2 = n / d
            val r = n - (n2 * d)
            val p1 = numberToWords(n2) + " " + s
            val p2 = if (r > 0) sep + numberToWords(r) else "" 
            return s"$p1$p2" 
        }

        def handleUnits = unitsToString(n) 

        def handleFirstDecade = firstDecadeToString(n) 

        def handleFirstHundred = {
            val sep = " "
            val units = n % 10
            val tens = n - units
            val p1 = tensToString(tens) 
            val p2 = if (units > 0) sep + unitsToString(units) else ""
            s"$p1$p2"
        }

        n match {
            case n if n < 1 => throw new IllegalArgumentException
            case n if n < 10 => handleUnits
            case n if n < 20 => handleFirstDecade   
            case n if n < 100 => handleFirstHundred
            case n if n < 1000 => helper(100, "hundred")   
            case n if n < 1000000 => helper(1000, "thousand")   
            case n if n < 1000000000 => helper(1000000, "million")
            case _ => throw new IllegalArgumentException   
        } 
    }

    def wordsToNumber(ws: String): Int = {
        1
    } 
}
