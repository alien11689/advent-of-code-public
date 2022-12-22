package pl.touk.dpr.aoc2022

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/21/input.txt")
//        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/21/test1.txt")))
        println(part1(lines))
//        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/21/test1.txt")))
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

        fun match(values: Map<String, Long>, result: Long): Pair<String, Long> {
            return if (left in values) {
                val v1 = values[left]!!
                when (operation) {
                    "+" -> Pair(right, result - v1) // v1 + x = res => x = res - v1
                    "-" -> Pair(right, v1 - result) // v1 - x = res => x = v1 - res
                    "*" -> Pair(right, result / v1) // v1 * x = result => x = result / v1
                    "/" -> Pair(right, v1 / result) // v1 / x = result => v1 = result * x => x = v1/result
                    else -> throw RuntimeException("unknown $left $right $operation")
                }
            } else if (right in values) {
                val v2 = values[right]!!
                when (operation) {
                    "+" -> Pair(left, result - v2) // x + v2 = res => x = res - v2
                    "-" -> Pair(left, result + v2) // x - v2 = res => x = res + v2
                    "*" -> Pair(left, result / v2) // x * v2 = res => x = res / v2
                    "/" -> Pair(left, result * v2) // x / v2 = res => x = res * v2
                    else -> throw RuntimeException("unknown $left $right $operation")
                }
            } else {
                throw RuntimeException("???")
            }
        }

        override fun toString(): String {
            return "($left $operation $right)"
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
        calculateSimpleValues(monkeys, values)
        return values["root"]!!
    }

    private fun part2(lines: List<String>): Any {
        val values = mutableMapOf<String, Long>()
        val monkeys = mutableMapOf<String, Monkey>()
        var rootComparison: Pair<String, String>? = null
        lines.forEach { line ->
            val (monkey, expression) = line.split(":").map { it.trim() }
            if (monkey != "humn") {
                val parts = expression.split(" ")
                if (parts.size == 1) {
                    values[monkey] = parts[0].toLong()
                } else {
                    if (monkey == "root") {
                        rootComparison = Pair(parts[0], parts[2])
                    } else monkeys[monkey] = Monkey(parts[0], parts[2], parts[1])
                }
            }
        }
        calculateSimpleValues(monkeys, values)
        var cur = monkeys[rootComparison!!.first]!!.match(values, values[rootComparison!!.second]!!)
        while (cur.first != "humn") {
//            println(cur)
            cur = monkeys[cur.first]!!.match(values, cur.second)
        }
        return cur.second
    }

    private fun calculateSimpleValues(monkeys: MutableMap<String, Monkey>, values: MutableMap<String, Long>) {
        while (true) {
            val monkeysToDelete = mutableSetOf<String>()
            monkeys.entries.forEach { (name, monkey) ->
                val res = monkey.calculate(values)
                if (res != null) {
                    values[name] = res
                    monkeysToDelete.add(name)
                }
            }
            monkeysToDelete.forEach { monkeys.remove(it) }
            if (monkeysToDelete.isEmpty()) {
                break
            }
        }
    }
}

