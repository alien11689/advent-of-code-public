package dpr.aoc2023

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getLinesFromFile("/13/input.txt")
//        val lines = Util.getLinesFromFile("/13/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        var i = 0
        var sum = 0L
        val rows = mutableListOf<String>()
        while (i < lines.size) {
            if (lines[i].isEmpty()) {
                val findPatchScore = findPatchScore(rows)
                sum += findPatchScore
                rows.clear()
            }else {
                rows.add(lines[i])
            }
            ++i
        }
//        sum += findPatchScore(rows)
        // 83315 is too high
        return sum
    }

    private fun findPatchScore(rows: MutableList<String>): Long {
        var resI = 0
        var resJ = 0
        var i = 0
        while (i < rows.size - 1) {
            if (rows[i] == rows[i + 1] && checkSymetry(rows, i, i + 1)) {
                resI = ++i
                break
            }
            ++i
        }
        val columns = transpose(rows)
        var j = 0
        while (j < columns.size - 1) {
            if (columns[j] == columns[j + 1] && checkSymetry(columns, j, j + 1)) {
                resJ = ++j
                break
            }
            ++j
        }
        return resI * 100L + resJ
    }

    private fun transpose(rows: MutableList<String>): List<String> {
        val res = mutableListOf<MutableList<Char>>()
        repeat(rows[0].length) {
            res.add(mutableListOf())
        }
        rows.forEach { r ->
            r.forEachIndexed { i, c ->

                res[i].add(c)
            }
        }
        return res.map { it.joinToString("") }.toList()
    }

    private fun checkSymetry(rows: List<String>, left: Int, right: Int): Boolean {
        var i = left
        var j = right
        while (i >= 0 && j < rows.size) {
            if (rows[i] != rows[j]) {
                return false
            }
            --i
            ++j
        }
        return true
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

