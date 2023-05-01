package dpr.eulerproject

object Problem0023 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val abundants = mutableSetOf<Int>()
        val sums = mutableSetOf<Int>()
        var sum = 0L
        (1..28123).forEach { cur ->
            if (cur !in sums) {
                sum += cur
            }
            if (Util.properDividers(cur).sum() > cur) {
                abundants.add(cur)
                sums.addAll(abundants.map { it + cur })
            }
        }
        println(sum)
    }
}
