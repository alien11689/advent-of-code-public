package pl.touk.dpr.eulerproject

object Problem0043 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val digits = ('0'..'9').toSet()
        val mutableMap = mutableMapOf<Pair<Set<Char>, Int>, Set<String>>()

        val primes = listOf(1,2,3,5,7,11,13,17)

        val res = permutations(digits, 10, mutableMap)
                .filter { pandigital ->
                    !pandigital.startsWith("0") && pandigital.windowed(3, 1, false)
                            .withIndex()
                            .all { v ->  v.value.toInt() % primes[v.index] == 0}
                }
                .sumOf {
//                    println("Found $it")
                    it.toLong()
                }

        println(res)
    }

    private fun permutations(digits: Set<Char>, size: Int,
                             permutationsMem: MutableMap<Pair<Set<Char>, Int>, Set<String>>): Set<String> {
        if (digits to size in permutationsMem) {
            return permutationsMem[digits to size]!!
        }
        if (size == 1) {
            return digits.map { it.toString() }.toSet()
        }
        val perms = digits.flatMap { digit ->
            permutations(digits - digit, size - 1, permutationsMem).map { it + digit }
        }.toSet()
        permutationsMem[digits to size] = perms
        return perms
    }
}
