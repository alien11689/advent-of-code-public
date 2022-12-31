package pl.touk.dpr.eulerproject

object Problem0032 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val digits = List(9) { (it + 1).toString().first() }.toSet()
        val mem = mutableSetOf<Set<List<Char>>>()
        val permutationsMem = mutableMapOf<Pair<Set<Char>, Int>, Set<List<Char>>>()
        val products = mutableSetOf<Long>()
        for (i in 4 downTo 1) {
            permutations(digits, i, permutationsMem).forEach { first ->
                for (j in 4 downTo 1) {
                    permutations(digits - first.toSet(), j, permutationsMem).forEach { second ->
                        val key = setOf(first, second)
                        if (key !in mem) {
                            val firstValue = first.joinToString("").toInt()
                            val secondValue = second.joinToString("").toInt()
                            val value = firstValue * secondValue
                            val valueSet = value.toString().toSet()
                            if (value.toString().length == valueSet.size && valueSet == digits - first.toSet() - second.toSet()) {
                                products.add(value.toLong())
//                                println("Found $value = $firstValue x $secondValue")
                            }
                            mem.add(key)
                        }
                    }
                }
            }
        }
        println(products.sum())
        //18044713 is wrong
    }

    private fun permutations(digits: Set<Char>, size: Int, permutationsMem: MutableMap<Pair<Set<Char>, Int>, Set<List<Char>>>): Set<List<Char>> {
        if (digits to size in permutationsMem) {
            return permutationsMem[digits to size]!!
        }
        if (size == 1) {
            return digits.map { listOf(it) }.toSet()
        }
        val perms = digits.flatMap { digit ->
            permutations(digits - digit, size - 1, permutationsMem).map { it + digit }
        }.toSet()
        permutationsMem[digits to size] = perms
        return perms
    }
}
