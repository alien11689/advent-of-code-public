package pl.touk.dpr.eulerproject

object Problem0033 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = (3..99999).fold(0L) { cur, num ->
            val value = num.toString().sumOf { factorial(it) }
            if(value == num){
                cur + num
            }else cur
        }
        println(res)
    }

    private fun factorial(number: Char): Int {
        return when (number){
            '0' -> 1
            '1' -> 1
            '2' -> 2
            '3' -> 6
            '4' -> 24
            '5' -> 120
            '6' -> 720
            '7' -> 5040
            '8' -> 40320
            '9' -> 362880
            else -> throw RuntimeException("$number")
        }
    }
}
