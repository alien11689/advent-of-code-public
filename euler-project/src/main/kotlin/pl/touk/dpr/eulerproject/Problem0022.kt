package pl.touk.dpr.eulerproject

object Problem0022 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val letterToNum = ('A'..'Z').zip(1..26).associate { it }
        val res = Util.getFileContent("/p022_names.txt")
                .split(",")
                .sorted()
                .mapIndexed { i, word ->
                    (i + 1L) * word.sumOf { letterToNum[it]?:0 }
                }
                .sum()
        println(res)
    }
}
