package pl.touk.dpr.aoc2022

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/21/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/21/test1.txt")))
        println(part1(lines))
        println("Part 2:")
        println(part2(Util.getNotEmptyLinesFromFile("/21/test1.txt")))
        println(part2(lines))
    }

    data class Monkey(val left: String, val right: String, val operation: String) {
        fun calculate(values: Map<String, Long>): Long? {
            return if (left in values && right in values) {
                when (operation) {
                    "+" -> values[left]!! + values[right]!!
                    "-" -> values[left]!! - values[right]!!
                    "*" -> values[left]!! * values[right]!!
                    "/" -> values[left]!! / values[right]!!
                    else -> throw RuntimeException("unknown $left $right $operation")
                }
            } else null
        }
    }

    private fun part1(lines: List<String>): Any {
        val values = mutableMapOf<String, Long>()
        val monkeys = mutableMapOf<String, Monkey>()
        lines.forEach { line ->
            val (monkey, expression) = line.split(":").map { it.trim() }
            val parts = expression.split(" ")
            if (parts.size == 1) {
                values[monkey] = parts[0].toLong()
            } else {
                monkeys[monkey] = Monkey(parts[0], parts[2], parts[1])
            }
        }
        while ("root" !in values) {
            val monkeysToDelete = mutableSetOf<String>()
            monkeys.entries.forEach { (name, monkey) ->
                val res = monkey.calculate(values)
                if (res != null) {
                    values[name] = res
                    monkeysToDelete.add(name)
                }
            }
            monkeysToDelete.forEach { monkeys.remove(it) }
        }
        return values["root"]!!
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

