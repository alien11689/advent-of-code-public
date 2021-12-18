package pl.touk.dpr.aoc2021

import java.util.Stack

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/18/input1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        lines.map { line -> readSnumber(line) }
//            .map { snumber -> snumber.reduce() }
            .forEach(::println)

        return -1
    }

    private fun readSnumber(line: String): Snumber {
        var i = 0
        val stack = Stack<Snumber>()
        while (i < line.length) {
            if (line[i] == '[') {
                stack.add(Snumber(null, listOf()))
            } else if (line[i] == ']') {
                val cur = stack.pop()
                if (stack.isEmpty()) {
                    println("line $line => $cur")
                    break
                } else {
                    val parent = stack.pop()
                    stack.push(parent.copy(values = parent.values + cur))
                }
            } else if (line[i] == ',') {
                val cur = stack.pop()
                val parent = stack.pop()
                stack.push(parent.copy(values = parent.values + cur))
            } else { // numbers
                val n = line[i].toString().toInt()
                stack.push(Snumber(n))
            }
            ++i
        }
        return stack.pop()
    }

    private fun part2(lines: List<String>): Any {
        return -1
    }

    data class Snumber(val value: Int?, val values: List<Snumber> = listOf(), val parent: Snumber? = null) {
        override fun toString(): String {
            if (value != null) {
                return value.toString()
            } else {
                return values.joinToString(",", "[", "]")
            }
        }

        fun isPair(): Boolean {
            return value == null && values.size == 2
        }

        fun isValue(): Boolean = value != null

        fun reduce(nestCount: Int = 0, left: Int? = null, right: Int? = null): Pair<Snumber, Boolean> {
            if (value != null) {
                if (value >= 10) {
                    return Pair(Snumber(null, listOf(Snumber(value / 2), Snumber((value + 1) / 2))), true)
                } else {
                    return Pair(this, false)
                }
            } else {
                var changed = false
                for (i in values.indices) {

                    if (changed)
                }
                if (cha)
            }
            TODO("Not yet implemented")
        }
    }
}


