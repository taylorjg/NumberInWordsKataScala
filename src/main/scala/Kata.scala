object Kata {

    private val unitsToString = Map(
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

    private val stringToUnits = Map(
        ("one" -> 1),
        ("two" -> 2),
        ("three" -> 3),
        ("four" -> 4),
        ("five" -> 5),
        ("six" -> 6),
        ("seven" -> 7),
        ("eight" -> 8),
        ("nine" -> 9))

    private val stringToFirstDecade = Map(
        ("ten" -> 10),
        ("eleven" -> 11),
        ("twelve" -> 12),
        ("thirteen" -> 13),
        ("fourteen" -> 14),
        ("fifteen" -> 15),
        ("sixteen" -> 16),
        ("seventeen" -> 17),
        ("eighteen" -> 18),
        ("nineteen" -> 19))

    private val stringToTens = Map(
        ("twenty" -> 20),
        ("thirty" -> 30),
        ("forty" -> 40),
        ("fifty" -> 50),
        ("sixty" -> 60),
        ("seventy" -> 70),
        ("eighty" -> 80),
        ("ninety" -> 90))

    def wordsToNumber(w: String): Int = {

        val ws = w.split(" ")

        if (ws.length > 1) {
            val v1 = stringToTens(ws(0))
            val v2 = stringToUnits(ws(1))
            return v1 + v2
        }

        def tryTens() = stringToTens(w)
        def tryFirstDecade() = stringToFirstDecade.getOrElse(w, tryTens())
        stringToUnits.getOrElse(w, tryFirstDecade())

        // split on " and "
        // e.g. "eight hundred and twenty two" => ["eight hundred", "twenty two"]

        // "eight hundred" => 800
        // "twenty two" => 22
        // sum (aggregate) 800 + 22 = 822

        // split on " "
        // e.g. "eight hundred" => ["eight", "hundred"]
        // e.g. "" => ["twenty", "two"]

        // look for "hundred" or "thousand" or "million"
        // if "hundred" found then (similar for "thousand" and "million"):
        //   parse the parts before "hundred" => "eight" => 8 (recursive ?)
        //   multiply by 100 => 8 * 100 => 800
        //   parse the rest => "twenty two" => 22 (recursive ?)
        //   add the bits => 800 + 22 => 822
    } 
}
