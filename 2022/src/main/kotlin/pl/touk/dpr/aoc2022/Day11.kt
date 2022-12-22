package pl.touk.dpr.aoc2022

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/11/input.txt")
        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/11/test1.txt")))
        println(part1(lines))
        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/11/test1.txt")))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val monkeys = readMonkeys(lines).toMap()
        return multipleTwoBiggestInspected(monkeys, 20) { a -> a / 3 }
    }

    private fun readMonkeys(lines: List<String>): MutableMap<Int, Monkey> {
        var i = 0
        val monkeys = mutableMapOf<Int, Monkey>()
        while (i < lines.size) {
            val id = lines[i++].split(" ").last().split(":")[0].toInt()
            val items = lines[i++].split(":").last().split(",").map { it.trim().toLong() }
            val operationParts = lines[i++].split("= old ").last().split(" ")
            val operation = when (operationParts[0]) {
                "+" -> when (operationParts[1]) {
                    "old" -> {
                        val f = { a: Long -> a + a }
                        f
                    }

                    else -> {
                        val v = operationParts[1].toLong()
                        val f = { a: Long -> a + v }
                        f
                    }
                }

                "*" -> when (operationParts[1]) {
                    "old" -> {
                        val f = { a: Long -> a * a }
                        f
                    }

                    else -> {
                        val v = operationParts[1].toLong()
                        val f = { a: Long -> a * v }
                        f
                    }
                }

                else -> throw RuntimeException("unknown $operationParts")
            }
            val test = lines[i++].split(" ").last().toLong()
            val ok = lines[i++].split(" ").last().toInt()
            val notOk = lines[i++].split(" ").last().toInt()
            // println("Monkey $id has items $items with operation $operationParts and if $test then $ok else $notOk")
            monkeys[id] = Monkey(id, items.toMutableList(), operation, test, ok, notOk)
        }
        return monkeys
    }

    private fun part2(lines: List<String>): Any {
        val monkeys = readMonkeys(lines).toMap()
        val base = monkeys.values.fold(1L) { acc, cur -> acc * cur.test }
        return multipleTwoBiggestInspected(monkeys, 10000) { a -> a % base }
    }

    private fun multipleTwoBiggestInspected(monkeys: Map<Int, Monkey>, iterations: Int, magnitudeModifier: (Long) -> Long): Long {
        repeat(iterations) {
            monkeys.values.sortedBy { it.id }.forEach { monkey ->
                monkey.play(monkeys, magnitudeModifier)
            }
        }
        return monkeys.values.map { it.inspected }.sortedDescending().take(2).reduce { acc, i -> acc * i }
    }

    data class Monkey(
        val id: Int,
        val items: MutableList<Long>,
        val operation: (a: Long) -> Long,
        val test: Long,
        val left: Int,
        val right: Int,
        var inspected: Long = 0L,
    ) {
        fun play(monkeys: Map<Int, Monkey>, magnitudeModifier: (Long) -> Long) {
            inspected += items.size
            items.forEach { item ->
                val newItem = magnitudeModifier(operation(item))
                val target = if (newItem % test == 0L) left else right
                monkeys[target]!!.items.add(newItem)
            }
            items.clear()
        }
    }
}

