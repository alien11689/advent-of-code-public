package dpr.aoc2023

import dpr.commons.Util

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
                sum += findScore(rows)
                rows.clear()
            } else {
                rows.add(lines[i])
            }
            ++i
        }
        // 83315 is too high
        return sum
    }

    private fun findScore(rows: MutableList<String>): Int {
        var i = 0
        while (i < rows.size - 1) {
            if (rows[i] == rows[i + 1] && checkSymmetry(rows, i, i + 1)) {
                return (++i) * 100
            }
            ++i
        }
        val columns = transpose(rows)
        var j = 0
        while (j < columns.size - 1) {
            if (columns[j] == columns[j + 1] && checkSymmetry(columns, j, j + 1)) {
                return ++j
            }
            ++j
        }
        throw RuntimeException("No symmetry")
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

    private fun checkSymmetry(rows: List<String>, left: Int, right: Int): Boolean {
        var i = left - 1
        var j = right + 1
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
        var i = 0
        var sum = 0L
        val rows = mutableListOf<String>()
        while (i < lines.size) {
            if (lines[i].isEmpty()) {
                sum += findSmudgeScore(rows)
                rows.clear()
            } else {
                rows.add(lines[i])
            }
            ++i
        }
        //29000 is too low
        return sum
    }

    private fun findSmudgeScore(rows: MutableList<String>): Int {
        var i = 0
        while (i < rows.size - 1) {
            if (isSmudge(rows[i], rows[i + 1]) && checkSymmetry(rows, i, i + 1) ||
                rows[i] == rows[i + 1] && checkSymmetryWithOneSmudge(rows, i, i + 1)
            ) {
                return (++i) * 100
            }
            ++i
        }
        val columns = transpose(rows)
        var j = 0
        while (j < columns.size - 1) {
            if (isSmudge(columns[j], columns[j + 1]) && checkSymmetry(columns, j, j + 1) ||
                columns[j] == columns[j + 1] && checkSymmetryWithOneSmudge(columns, j, j + 1)
            ) {
                return ++j
            }
            ++j
        }
        throw RuntimeException("No smudge symmetry")
    }

    private fun checkSymmetryWithOneSmudge(rows: List<String>, left: Int, right: Int): Boolean {
        var smudge = false
        var i = left - 1
        var j = right + 1
        while (i >= 0 && j < rows.size) {
            if (rows[i] != rows[j]) {
                if (!smudge && isSmudge(rows[i], rows[j])) {
                    smudge = true
                } else {
                    return false
                }
            }
            --i
            ++j
        }
        return smudge
    }

    private fun isSmudge(a: String, b: String): Boolean {
        return a.zip(b).count { (a, b) -> a != b } == 1
    }
}

