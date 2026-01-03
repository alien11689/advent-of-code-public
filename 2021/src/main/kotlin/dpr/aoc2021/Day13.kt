package dpr.aoc2021

import dpr.commons.Util
import dpr.commons.Point2D as Point

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/13/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): Int {
        val (instructions, paper) = readInput(lines)
        val newPaper = fold(paper, instructions.first())
        return newPaper.size
    }

    private fun fold(paper: Set<Point>, instr: Fold): Set<Point> {
        val result = mutableSetOf<Point>()
        if (instr.axis == "x") {
            paper.forEach { p ->
                result += if (p.x < instr.idx) {
                    p
                } else {
                    Point(instr.idx - (p.x - instr.idx), p.y)
                }
            }
        } else {
            paper.forEach { p ->
                result += if (p.y < instr.idx) {
                    p
                } else {
                    Point(p.x, instr.idx - (p.y - instr.idx))
                }
            }
        }
        return result.toSet()
    }

    data class Fold(val axis: String, val idx: Int)

    @JvmStatic
    fun part2(lines: List<String>): String {
        val (instructions, paper) = readInput(lines)

        val newPaper = instructions.fold(paper.toSet()) { acc, it -> fold(acc, it) }
        val maxY = newPaper.maxOf { it.y }
        val maxX = newPaper.maxOf { it.x }
        val sb = StringBuilder()
        for (i in 0..maxY) {
            for (j in 0..maxX) {
                sb.append(if (Point(j, i) in newPaper) '#' else ' ')
            }
            if (i < maxY) {
                sb.append('\n')
            }
        }
        return sb.toString()
    }

    private fun readInput(lines: List<String>): Pair<List<Fold>, Set<Point>> {
        val instructions = mutableListOf<Fold>()
        val paper = mutableSetOf<Point>()
        lines.forEach { line ->
            if (line.startsWith("fold")) {
                val sp = line.split(" ")[2].split("=")
                instructions.add(Fold(sp[0], sp[1].toInt()))
            } else {
                val sp = line.split(",")
                paper.add(Point(sp[0].toInt(), sp[1].toInt()))
            }
        }
        return Pair(instructions, paper)
    }
}

