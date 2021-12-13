package pl.touk.dpr.aoc2021

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/13/input.txt")
        println(part1(lines))
        part2(lines)
    }

    private fun part1(lines: List<String>): Any {
        val (instructions, paper) = readInput(lines)
        val newPaper = fold(paper, instructions.first())
        return newPaper.size
    }

    private fun fold(paper: Set<Point>, instr: Fold): Set<Point> {
        val result = mutableSetOf<Point>()
        if (instr.axis == "x") {
            paper.forEach { p ->
                if (p.x < instr.idx) {
                    result += p
                } else {
                    result += Point(instr.idx - (p.x - instr.idx), p.y)
                }
            }
        } else {
            paper.forEach { p ->
                if (p.y < instr.idx) {
                    result += p
                } else {
                    result += Point(p.x, instr.idx - (p.y - instr.idx))
                }
            }
        }
        return result.toSet()
    }

    data class Point(val x: Int, val y: Int)

    data class Fold(val axis: String, val idx: Int)

    private fun part2(lines: List<String>): Any {
        val (instructions, paper) = readInput(lines)

        val newPaper = instructions.fold(paper.toSet()) { acc, it -> fold(acc, it) }
        val maxY = newPaper.maxOf { it.y }
        val maxX = newPaper.maxOf { it.x }
        for (i in 0..maxY) {
            for (j in 0..maxX) {
                print(if (Point(j, i) in newPaper) '#' else ' ')
            }
            println()
        }
        return -1
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

