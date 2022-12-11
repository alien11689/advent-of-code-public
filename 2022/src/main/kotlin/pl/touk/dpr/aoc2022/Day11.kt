package pl.touk.dpr.aoc2022

import java.math.BigInteger

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/11/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/11/test1.txt")))
        println(part1(lines))
        println("Part 2:")
        println(part2(Util.getNotEmptyLinesFromFile("/11/test1.txt")))
        println(part2(lines))
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

    data class Item(val id: Int, val value: BigInteger)

    private fun readMonkeys(lines: List<String>): MutableMap<Int, Monkey> {
        var i = 0
        var itemId = 0
        val monkeys = mutableMapOf<Int, Monkey>()
        while (i < lines.size) {
            val id = lines[i].split(" ").last().split(":")[0].toInt()
            i++
            val items = lines[i].split(":")[1].split(",").map { it.trim().toBigInteger() }
                .map { Item(itemId++, it) }
            i++
            val operationParts = lines[i].split("= old ")[1].split(" ")
            val operation = when (operationParts[0]) {
                "+" -> when (operationParts[1]) {
                    "old" -> {
                        val f = { a: BigInteger -> a + a }
                        f
                    }

                    else -> {
                        val v = operationParts[1].toBigInteger()
                        val f = { a: BigInteger -> a + v }
                        f
                    }
                }

                "*" -> when (operationParts[1]) {
                    "old" -> {
                        val f = { a: BigInteger -> a * a }
                        f
                    }

                    else -> {
                        val v = operationParts[1].toBigInteger()
                        val f = { a: BigInteger -> a * v }
                        f
                    }
                }

                else -> throw RuntimeException("unknown $operationParts")
            }
            i++
            val test = lines[i].split(" ").last().toBigInteger()
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
        val base = monkeys.values.map { it.test }.reduce{ acc, cur -> acc.multiply(cur)}
        println("Base is $base")
//        val mem = mutableMapOf<Map<Int, List<Int>>, Int>()
        (1..10000).forEach {
            monkeys.values.sortedBy { it.id }.forEach { monkey ->
                monkey.play2(monkeys, base)
            }
//            val key = monkeys.map { it.key to it.value.items.map { it.id }.toList() }.toMap()
//            if (key in mem) {
//                println("Cycle detected for round $it seen in ${mem[key]}")
//                println("  Values: ${monkeys.map { it.key to it.value.items.map { it.value }.toList() }.toMap()}")
//            }
//            mem.put(key, it)
            if (it == 1 || it == 20 || it % 1000 == 0) {
                println("Round $it has monkeys ${monkeys.values.map { it.inspected }}")
            }
        }
        return monkeys.values.map { it.inspected }.sortedDescending().take(2).reduce { acc, i -> acc * i }
    }

    data class Monkey(
        val id: Int,
        val items: MutableList<Item>,
        val operation: (a: BigInteger) -> BigInteger,
        val test: BigInteger,
        val left: Int,
        val right: Int,
        var inspected: Long = 0L,
    ) {
        fun play(monkeys: Map<Int, Monkey>) {
            inspected += items.size
            items.forEach { item ->
                val newItem = operation(item.value).div(3.toBigInteger())
                val itemAfterOperation = item.copy(value = newItem)
                if (newItem.mod(test) == BigInteger.ZERO) {
                    monkeys[left]!!.items.add(itemAfterOperation)
                } else {
                    monkeys[right]!!.items.add(itemAfterOperation)
                }
            }
            items.clear()
        }

        fun play2(monkeys: Map<Int, Monkey>, base: BigInteger) {
            inspected += items.size
            items.forEach { item ->
                val newItem = operation(item.value).mod(base)
                val itemAfterOperation = item.copy(value = newItem)
                if (newItem.mod(test) == BigInteger.ZERO) {
                    monkeys[left]!!.items.add(itemAfterOperation)
                } else {
                    monkeys[right]!!.items.add(itemAfterOperation)
                }
            }
            items.clear()
        }
    }
}

