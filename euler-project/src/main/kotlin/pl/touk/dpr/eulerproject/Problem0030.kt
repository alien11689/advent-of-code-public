package pl.touk.dpr.eulerproject

object Problem0030 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val sums = mutableMapOf(0L to 0L, 1L to 1L)
        var sum = 0L
        (2..10000000L).forEach { a ->
//            println("Checking $a")
            val x = if (a in 2..9) {
                a * a * a * a * a
            } else {
                val first = a.toString().first().toString().toLong()
                first * first * first * first * first + sums[a.toString().drop(1).toLong()]!!
            }
            sums[a] = x
            if (x == a) {
                sum += a
//                println("Found $a, cur sum is $sum")
            }
        }
        println(sum)
    }
}
