package pl.touk.dpr.eulerproject

object Problem0018 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val triangle = """
            75
            95 64
            17 47 82
            18 35 87 10
            20 04 82 47 65
            19 01 23 75 03 34
            88 02 77 73 07 63 67
            99 65 04 28 06 16 70 92
            41 41 26 56 83 40 80 70 33
            41 48 72 33 47 32 37 16 94 29
            53 71 44 65 25 43 91 52 97 51 14
            70 11 33 28 77 73 17 78 39 68 17 57
            91 71 52 38 17 14 91 43 58 50 27 29 48
            63 66 04 68 89 53 67 30 73 16 69 87 40 31
            04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
        """.trimIndent().lines().map { line -> line.split(" ").map { it.toInt() } }
        println(maxPath(0, 0, triangle, mutableMapOf()))
    }

    private fun maxPath(row: Int, column: Int, triangle: List<List<Int>>, mem: MutableMap<Pair<Int, Int>, Int>): Int {
        val key = row to column
        if (key in mem) {
            return mem[key]!!
        }
        var length = triangle[row][column]
        if (row + 1 == triangle.size) {
            return length
        }
        val leftPath = maxPath(row + 1, column, triangle, mem)
        val rightPath = maxPath(row + 1, column + 1, triangle, mem)
        length += if (leftPath > rightPath) leftPath else rightPath
        mem[key] = length
        return length
    }
}
