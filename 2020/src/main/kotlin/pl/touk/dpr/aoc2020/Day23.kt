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
        val pair = buildNodes(cups)
        var current: Node? = pair.first
        val mem = pair.second
        val max = mem.keys.maxOrNull()!!
        (1..100).forEach {
            current = move(current!!, max, mem)
        }
        val node1: Node = mem[1]!!
        current = node1.next!!
        val res = StringBuilder()
        while (current!!.value != 1) {
            res.append(current!!.value)
            current = current!!.next
        }
        return res.toString()
    }

    private fun part2(input: String): Any {
        val initCups = input.map { it.toString().toInt() }.toList()
        val elements = initCups + ((initCups.maxOrNull()!! + 1)..1000000)
        val pair = buildNodes(elements)
        var current: Node? = pair.first
        val mem = pair.second
        val max = mem.keys.maxOrNull()!!
        (1..10000000).forEach {
            current = move(current!!, max, mem)
        }
        val node1: Node = mem[1]!!
        return node1.next!!.value.toLong() * node1.next!!.next!!.value.toLong()
    }

    private fun buildNodes(elements: List<Int>): Pair<Node, Map<Int, Node>> {
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
        return Pair(current!!, mem)
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
