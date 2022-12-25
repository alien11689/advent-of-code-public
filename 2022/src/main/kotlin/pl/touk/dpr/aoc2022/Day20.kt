package pl.touk.dpr.aoc2022

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/20/input.txt")
//        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/20/test1.txt")))
        println(part1(lines))
//        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/20/test1.txt")))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val elements = processElements(parseInput(lines, 1L).toMutableMap())
        return calculateFinalScore(elements)
    }

    data class Elem(val v: Long, val origin: Long)

    private fun part2(lines: List<String>): Any {
        val encryptionKey = 811589153L
        val elements = parseInput(lines, encryptionKey).toMutableMap()
        repeat(10) {
            processElements(elements)
        }
        return calculateFinalScore(elements)
    }

    private fun processElements(elements: MutableMap<Long, Elem>): Map<Long, Elem> {
        // mutable map instead of immutable speed up ~4-5 times execution
        val maxIdx = (elements.size - 1).toLong()
        repeat(elements.size) { i ->
            val curElem = elements.entries.first { it.value.origin == i.toLong() }
            val curPos = curElem.key
            val value = curElem.value
            val shift = value.v
            val targetPos: Long = if (shift > 0) {
                if (curPos + shift <= maxIdx) {
                    curPos + shift
                } else {
                    (curPos + shift) % maxIdx
                }
            } else if (shift < 0) {
                if (curPos + shift > 0) {
                    curPos + shift
                } else if (curPos + shift == 0L) {
                    maxIdx
                } else {
                    (maxIdx * 10000000000 + curPos + shift) % maxIdx
                }
            } else curPos
            //                println("Applying $curElem: target pos is $targetPos and curPos is $curPos")
//            require(targetPos in elements.keys)
            if (curPos < targetPos) {
                for (j in curPos until targetPos) {
                    elements[j] = elements[j + 1]!!
                }
                elements[targetPos] = value
            } else if (curPos > targetPos) {
                for (j in curPos downTo (targetPos + 1)) {
                    elements[j] = elements[j - 1]!!
                }
                elements[targetPos] = value
            }
            //            println(elements.entries.sortedBy { it.key }.map { it.value.v }.joinToString(", "))
            //            println()
        }
        return elements
    }

    private fun calculateFinalScore(elements: Map<Long, Elem>): Long {
        val origin = elements.entries.first { it.value.v == 0L }.key
        return elements[(origin + 1000) % elements.size]!!.v + elements[(origin + 2000) % elements.size]!!.v + elements[(origin + 3000) % elements.size]!!.v
    }

    private fun parseInput(lines: List<String>, encryptionKey: Long) =
        lines.mapIndexed { i, v -> i.toLong() to Elem(v.toLong() * encryptionKey, i.toLong()) }.toMap()
}

