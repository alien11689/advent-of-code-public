package pl.touk.dpr.eulerproject

object Problem0041 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val digits = '1'..'9'
        val mutableMap = mutableMapOf<Pair<Set<Char>, Int>, Set<String>>()
        for (i in 9 downTo 1) {
            val value = permutations(digits.take(i).toSet(), i, mutableMap)
                    .sortedDescending()
                    .firstOrNull { Util.isPrime(it.toLong()) }
            if (value != null) {
                println(value)
                break
            }
        }
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
