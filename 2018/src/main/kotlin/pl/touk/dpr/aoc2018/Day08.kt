package pl.touk.dpr.aoc2018

object Day08 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/08/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val root = readNode(input.split(" ").map { it.toInt() }.toMutableList())
        val toCheck = mutableListOf(root)
        var metadatadSum = 0L
        while (toCheck.isNotEmpty()) {
            val node = toCheck.removeFirst()
            toCheck.addAll(node.children)
            metadatadSum += node.metadata.sum()
        }
        return metadatadSum
    }

    private fun part2(input: String): Any {
        val root = readNode(input.split(" ").map { it.toInt() }.toMutableList())
        return root.calculateMeta()
    }

    data class Node(
        val childrenCount: Int,
        val metadataCount: Int,
        val metadata: MutableList<Int> = mutableListOf(),
        val children: MutableList<Node> = mutableListOf(),
        var value: Int? = null
    ) {

        fun calculateMeta(): Int {
            if (value != null) {
                return value!!
            }
            value = if (metadataCount == 0) {
                0
            } else if (childrenCount == 0) {
                metadata.sum()
            } else {
                metadata.filter { it <= childrenCount }
                        .sumOf { children[it - 1].calculateMeta() }
            }
            return value!!
        }
    }

    private fun readNode(values: MutableList<Int>): Node {
        val childrenCount = values.removeFirst()
        val metadataCount = values.removeFirst()
        val node = Node(childrenCount = childrenCount, metadataCount = metadataCount)
        repeat(childrenCount) {
            val child = readNode(values)
            node.children.add(child)
        }
        repeat(metadataCount) {
            val metadata = values.removeFirst()
            node.metadata.add(metadata)
        }
        return node
    }
}
