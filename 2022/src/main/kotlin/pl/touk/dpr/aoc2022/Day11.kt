package pl.touk.dpr.aoc2022

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/11/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/11/test1.txt")))
        println(part1(lines))
        println("Part 2:")
        println(part2(Util.getNotEmptyLinesFromFile("/11/test1.txt")))
//        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val monkeys = readMonkeys(lines).toMap()
        (1..20).forEach {
            monkeys.values.sortedBy { it.id }.forEach { monkey ->
                monkey.play(monkeys)
            }
        }
        return monkeys.values.map { it.inspected }.sortedDescending().take(2).reduce { acc, i -> acc * i }
    }

    private fun readMonkeys(lines: List<String>): MutableMap<Int, Monkey> {
        var i = 0
        val monkeys = mutableMapOf<Int, Monkey>()
        while (i < lines.size) {
            val id = lines[i].split(" ").last().split(":")[0].toInt()
            i++
            val items = lines[i].split(":")[1].split(",").map { it.trim().toLong() }
            i++
            val operationParts = lines[i].split("= old ")[1].split(" ")
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
                        val v = operationParts[1].toInt()
                        val f = { a: Long -> a * v }
                        f
                    }
                }

                else -> throw RuntimeException("unknown $operationParts")
            }
            i++
            val test = lines[i].split(" ").last().toInt()
            i++
            val ok = lines[i].split(" ").last().toInt()
            i++
            val notOk = lines[i].split(" ").last().toInt()
            i++
            //            println("Monkey $id has items $items with operation $operationParts and if $test then $ok else $notOk")
            monkeys[id] = Monkey(id, items.toMutableList(), operation, test, ok, notOk)
        }
        return monkeys
    }

    private fun part2(lines: List<String>): Any {
        val monkeys = readMonkeys(lines).toMap()
        (1..10000).forEach {
            monkeys.values.sortedBy { it.id }.forEach { monkey ->
                monkey.play2(monkeys)
            }
            if (it == 1 || it == 20 || it % 1000 == 0) {
                println("Round $it has monkeys ${monkeys.values.map { it.inspected }}")
            }
        }
        return monkeys.values.map { it.inspected }.sortedDescending().take(2).reduce { acc, i -> acc * i }
    }

    data class Monkey(
        val id: Int,
        val items: MutableList<Long>,
        val operation: (a: Long) -> Long,
        val test: Int,
        val left: Int,
        val right: Int,
        var inspected: Long = 0L,
    ) {
        fun play(monkeys: Map<Int, Monkey>) {
            inspected += items.size
            items.forEach { item ->
                val newItem = operation(item) / 3
                if (newItem % test == 0L) {
                    monkeys[left]!!.items.add(newItem)
                } else {
                    monkeys[right]!!.items.add(newItem)
                }
            }
            items.clear()
        }

        fun play2(monkeys: Map<Int, Monkey>) {
            inspected += items.size
            items.forEach { item ->
                val newItem = operation(item)
                if (newItem % test == 0L) {
                    monkeys[left]!!.items.add(newItem)
                } else {
                    monkeys[right]!!.items.add(newItem)
                }
            }
            items.clear()
        }
    }
}

