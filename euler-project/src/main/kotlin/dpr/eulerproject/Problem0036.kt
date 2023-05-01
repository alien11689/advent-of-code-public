package dpr.eulerproject

object Problem0036 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = generateSequence(1) { it + 2 }
                .takeWhile { it < 1_000_000 }
                .sumOf {
                    val numAsString = it.toString()
                    if (numAsString == numAsString.reversed() &&
                            Integer.toBinaryString(it) == Integer.toBinaryString(it).reversed()) {
//                        println(numAsString + " and " + Integer.toBinaryString(it))
                        it
                    } else 0
                }

        println(res)
        //545045040 is wrong
        // 302802800 is wrong
    }
}
