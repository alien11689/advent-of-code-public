package pl.touk.dpr.eulerproject

object Problem0007 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = generateSequence(2L) { it + 1 }
                .filter { Util.isPrime(it) }
                .drop(10000)
                .first()
        println(res)
    }
}
