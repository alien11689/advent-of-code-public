package pl.touk.dpr.eulerproject

object Problem0041 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val digits = '1'..'9'
        val prime = generateSequence(987_654_321L) {it - 2}
                .first {
                    val asString = it.toString()
                    val asCharSet = asString.toSet()
                    asString.length == asCharSet.size && asCharSet == digits.take(asString.length).toSet() && Util.isPrime(it)
                }

        println(prime)
    }
}
