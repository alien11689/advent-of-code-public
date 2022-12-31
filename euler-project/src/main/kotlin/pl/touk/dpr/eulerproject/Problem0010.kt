package pl.touk.dpr.eulerproject

object Problem0010 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = generateSequence(2L) { it + 1 }
                .takeWhile { it < 2_000_000 }
                .filter { Util.isPrime(it) }
                .sum()
        println(res)
    }
}
