package pl.touk.dpr.eulerproject

object Problem0034 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val fractions = (11..98)
                .filter { it % 10 != 0 }
                .flatMap { n ->
                    (n..99).filter { it > n && it % 10 != 0 }.map { n to it }
                }
                .flatMap {
                    val digitsInBoth = digits(it.first).intersect(digits(it.second))
                    if (digitsInBoth.isEmpty()) {
                        emptyList<Pair<Int, Int>>()
                    } else {
                        digitsInBoth.map { digit ->
                            val newX = (digits(it.first) - digit).firstOrNull() ?: digit
                            val newY = (digits(it.second) - digit).firstOrNull() ?: digit
                            if (it.first * newY == newX * it.second) newX to newY else null
                        }
                    }
                }
                .filterNotNull()
                .toSet()
//        println(fractions)
        val res = fractions.reduce { acc, cur ->
            acc.first * cur.first to acc.second * cur.second
        }
//        println(res)
        println(res.second / res.first) // simplification
    }

    private fun digits(number: Int): Set<Int> {
        return setOf(number / 10, number % 10)
    }
}
