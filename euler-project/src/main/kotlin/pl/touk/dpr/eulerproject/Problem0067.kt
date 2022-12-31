package pl.touk.dpr.eulerproject

object Problem0067 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val triangle = Util.getNotEmptyLinesFromFile("/p067_triangle.txt")
                .map { line -> line.split(" ").map { it.toInt() } }

        println(Problem0018.maxPath(0, 0, triangle, mutableMapOf()))
    }
}
