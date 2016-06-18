object Kata {

    private val SEP_SPACE = " "
    private val SEP_AND = " and "

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

        def helper(divisor: Int, qualifier: String): String = {
            val n2 = n / divisor
            val remainder = n - (n2 * divisor)
            val p1 = numberToWords(n2) + SEP_SPACE + qualifier
            val p2 = if (remainder > 0) SEP_AND + numberToWords(remainder) else "" 
            return s"$p1$p2" 
        }

        def handleUnits = unitsToString(n) 

        def handleFirstDecade = firstDecadeToString(n) 

        def handleFirstHundred = {
            val units = n % 10
            val tens = n - units
            val p1 = tensToString(tens) 
            val p2 = if (units > 0) SEP_SPACE + unitsToString(units) else ""
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

    private def dropQualifier(s: String) = s.split(SEP_SPACE).init.mkString(SEP_SPACE)

    def wordsToNumber(s: String): Int = {

        if (s.length == 0) return 0

        val andChunks = s.split(SEP_AND)
        val firstAndChunk = andChunks(0)
        val remainderString = andChunks.tail.mkString(SEP_AND)
        val remainderValue = wordsToNumber(remainderString) 

        val ts = List(
            ("million", 1000000),
            ("thousand", 1000),
            ("hundred", 100))

        for (t <- ts) {
            if (firstAndChunk.endsWith(t._1)) {
                val stringWithoutQualifier = dropQualifier(firstAndChunk)
                val value = wordsToNumber(stringWithoutQualifier) * t._2
                return value + remainderValue
            }
        } 

        val ws = s.split(SEP_SPACE)

        if (ws.length == 2) {
            val tensValue = stringToTens(ws(0))
            val unitsValue = stringToUnits(ws(1))
            return tensValue + unitsValue
        }

        if (ws.length == 1) {
            val value = stringToTens.getOrElse(s,
                stringToFirstDecade.getOrElse(s,
                    stringToUnits.getOrElse(s, 0)))
            if (value > 0) return value
        }

        throw new IllegalArgumentException
    } 
}
