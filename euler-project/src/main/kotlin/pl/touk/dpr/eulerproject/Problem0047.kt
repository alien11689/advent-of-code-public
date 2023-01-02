package pl.touk.dpr.eulerproject

object Problem0047 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val primes = mutableListOf(2L,3L)
        var i = 3L
        var prevHavingDesiredNumberOfFactors = 0
        val desired = 4
        while (true){
            ++i
            if (factors(i, primes).toSet().size == desired){
                prevHavingDesiredNumberOfFactors++
                if (prevHavingDesiredNumberOfFactors == desired){
                    println(i - desired + 1)
                    break
                }
            } else {
                prevHavingDesiredNumberOfFactors = 0
            }
        }
    }

    private fun factors(i: Long, primes: MutableList<Long>): List<Long> {
        var num = i
        val res = mutableListOf<Long>()
        for (p in primes){
            while(num % p == 0L){
                res.add(p)
                num /= p
            }
            if (num == 1L){
                return res
            }
        }
        if (res.isEmpty()){
            primes.add(i)
            println("Found prime $i")
        }
        return res
    }
}
