package dpr.eulerproject

object Problem0035 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val primes = (2 until 1_000_000)
                .filter { Util.isPrime(it.toLong()) }
                .map { it.toString() }
                .toSet()

        val res = mutableSetOf<String>()

        primes.forEach { prime ->
            when {
                prime.length == 1 -> res.add(prime)
                prime in res -> {}
                else -> {
                    val primeAsString = prime.toString()
                    val doublePrime = primeAsString + primeAsString
                    val possiblePrimes = primeAsString.indices.map { doublePrime.substring(it, it + primeAsString.length) }
                    if (primes.containsAll(possiblePrimes)) {
                        res.addAll(possiblePrimes)
                    }
                }
            }
        }

        println(res.size)
    }
}
