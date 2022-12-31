package pl.touk.dpr.eulerproject

object Problem0024 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val ranges = ('0'..'9').toMutableList()
        var nth = 1_000_000
        var i = 10
        val sb = StringBuilder()
        while (i >= 1) {
            if (ranges.size == 3) {
                // without this hack last three numbers are messed up...
//                println("Get permutation $nth from $ranges")
                sb.append(permutations(ranges)[nth - 1])
                break
            }
            val pos = (1 until i).fold(1) { acc, l -> acc * l }
            val index = nth / pos
            sb.append(ranges[index])
//            println("Using index $index ($pos) of $ranges and cur number is ${sb.toString()} // nth is $nth",)
            ranges.removeAt(index)
            nth -= index * pos
            --i
        }
        println(sb.toString())
    }

    private fun permutations(l: MutableList<Char>): List<String> = listOf(
            listOf(l[0], l[1], l[2]).joinToString(""),
            listOf(l[0], l[2], l[1]).joinToString(""),
            listOf(l[1], l[0], l[2]).joinToString(""),
            listOf(l[1], l[2], l[0]).joinToString(""),
            listOf(l[2], l[0], l[1]).joinToString(""),
            listOf(l[2], l[1], l[0]).joinToString(""),
    )
}
