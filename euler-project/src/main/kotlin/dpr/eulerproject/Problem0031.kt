package dpr.eulerproject

object Problem0031 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val coins = listOf(200, 100, 50, 20, 10, 5, 2, 1)
        val mem = mutableMapOf<Pair<Int, List<Int>>, Int>()
        println(groups(200, coins, mem))
    }

    private fun groups(amount: Int, coins: List<Int>, mem: MutableMap<Pair<Int, List<Int>>, Int>): Int {
        val key = amount to coins
        if (key in mem) {
            return mem[key]!!
        }
        val x = if (coins.isEmpty() || amount < 0) {
            0
        } else if (amount == 0) {
            1
        } else {
            val first = coins.first()
            val tail = coins.drop(1)
            groups(amount - first, coins, mem) + groups(amount, tail, mem)
        }
        mem[key] = x
        return x
    }
}
