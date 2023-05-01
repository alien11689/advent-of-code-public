package dpr.eulerproject

object Problem0049 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val primes = generateSequence(1000L) { it + 1 }
                .filter { Util.isPrime(it) }
                .takeWhile { it < 10000 }
                .toList()

        val groupedByDigits = primes.groupBy { it.toString().toList().sorted() }
        val values = groupedByDigits.filter { it.value.size >= 3 }.values
        values.forEach {
            for (x in it.indices) {
                if (x < it.size - 2) {
                    if (it[x] == 1487L) {
                        continue
                    }
                    val first = it[x]
                    for (y in it.indices) {
                        if (x < y) {
                            val second = it[y]
                            val third = second + second - first
                            if (third in it) {
                                println("$first$second$third")
                                return@measureTime
                            }
                        }
                    }
                }
            }
        }
    }
}
