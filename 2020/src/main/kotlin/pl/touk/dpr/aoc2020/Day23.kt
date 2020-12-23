package pl.touk.dpr.aoc2020

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = "186524973"
//        val input = "389125467" //sample
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        var cups = input.map { it.toString().toInt() }.toList()
        (1..100).forEach {
            cups = round(cups)
        }
        val res = cups.subList(cups.indexOf(1) + 1, cups.size) + cups.subList(0, cups.indexOf(1))

        return res.joinToString("") { it.toString() }
    }

    private fun round(cups: List<Int>): List<Int> {
        val curVal = cups[0]
        val pickedUp = (1..3).map { cups[it % cups.size] }
        var destination = if (curVal == 1) 9 else (curVal - 1)
        while (destination in pickedUp) {
            destination = if (destination == 1) 9 else (destination - 1)
        }
        val cupsNew = cups.filter { it != curVal && it !in pickedUp }
        val ml = ArrayList<Int>(cups.size)
        cupsNew.forEach {
            ml.add(it)
            if (it == destination) {
                ml.addAll(pickedUp)
            }
        }
        ml.add(curVal)
        return ml.toList()
    }

    private fun part2(input: String): Any {
        val initCups = input.map { it.toString().toInt() }.toList()
        val elements = initCups + ((initCups.max()!! + 1)..1000000)
        var begin: Node? = null
        var current: Node? = null
        val mem = mutableMapOf<Int, Node>()
        elements.forEach {
            val next = Node(it, null)
            mem[it] = next
            if (current == null) {
                current = next
                begin = next
            } else {
                current!!.next = next
                current = next
            }
        }
        current!!.next = begin
        current = begin
        val max = 1000000
        (1..10000000).forEach {
            current = move(current!!, max, mem)
        }
        val node1: Node = mem[1]!!
        return node1.next!!.value.toLong() * node1.next!!.next!!.value.toLong()
    }

    private fun move(current: Node, max: Int, mem: Map<Int, Node>): Node {
        val pickedUpNodeBegin = current.next!!
        val pickedUpValues = setOf(current.next!!.value, current.next!!.next!!.value, current.next!!.next!!.next!!.value)
        val nextNode = current.next!!.next!!.next!!.next!!
        var destination = if (current.value == 1) max else (current.value - 1)
        while (destination in pickedUpValues) {
            destination = if (destination == 1) max else (destination - 1)
        }
        val destinationNode: Node = mem[destination]!!
        current.next = nextNode
        val afterDestinationNode = destinationNode.next
        destinationNode.next = pickedUpNodeBegin
        pickedUpNodeBegin.next!!.next!!.next = afterDestinationNode
        return nextNode
    }

    data class Node(val value: Int, var next: Node? = null)
}
