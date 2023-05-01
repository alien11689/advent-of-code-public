package dpr.eulerproject

object Problem0040 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val sb = StringBuilder()
        var i = 1
        while (sb.length < 1000000) {
            sb.append(i++)
        }
        val s = sb.toString()
        val res = listOf(10, 100, 1000, 10_000, 100_000, 1_000_000).fold(1) { acc, n ->
            acc * s[n - 1].digitToInt()
        }

        println(res)
    }
}
