package pl.touk.dpr.eulerproject

object Problem0042 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val triangles = (1..26 * 3).map { it * (it + 1) / 2 }.toSet()
        val numToLetter = ('A'..'Z').zip(1..26).associate { it }
        val res = Util.getFileContent("/p042_words.txt")
                .split(",")
                .count { word -> word.sumOf { numToLetter[it] ?: 0 } in triangles }
        println(res)
    }
}
