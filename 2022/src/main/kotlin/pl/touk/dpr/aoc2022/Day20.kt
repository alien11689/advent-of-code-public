package pl.touk.dpr.aoc2022

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/20/input.txt")
        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/20/test1.txt")))
        println(part1(lines))
        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/20/test1.txt")))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val elements = processElements(parseInput(lines, 1L))
        return calculateFinalScore(elements)
    }

    data class Elem(val v: Long, val origin: Long)

    private fun part2(lines: List<String>): Any {
        val encryptionKey = 811589153L
        var elements = parseInput(lines, encryptionKey)
        repeat(10) {
            elements = processElements(elements)
        }
        return calculateFinalScore(elements)
    }

    private fun processElements(initialElements: Map<Long, Elem>): Map<Long, Elem> {
        var elements = initialElements
        val maxIdx = (elements.size - 1).toLong()
        (0 until elements.size).forEach { i ->
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
            require(targetPos in elements.keys)
            elements = if (curPos < targetPos) {
                elements.map {
                    val x = when {
                        it.key < curPos || it.key > targetPos -> it.key to it.value
                        it.key == targetPos -> it.key to value
                        else -> it.key to elements[it.key + 1]!!
                    }
                    //                    println("Moving $it to $x")
                    x
                }.toMap()
            } else if (curPos > targetPos) {
                elements.map {
                    val x = when {
                        it.key > curPos || it.key < targetPos -> it.key to it.value
                        it.key == targetPos -> it.key to value
                        else -> it.key to elements[it.key - 1]!!
                    }
                    //                    println("Moving $it to $x")
                    x
                }.toMap()
            } else elements
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

