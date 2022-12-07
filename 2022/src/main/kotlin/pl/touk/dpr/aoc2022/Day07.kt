package pl.touk.dpr.aoc2022

import java.util.Stack

object Day07 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/07/input.txt")
        println("Part 1:")
        println(part1(lines))
        println("Part 2:")
        println(part2(lines))
    }

    sealed interface Element {
        val name: String
        fun calculateSize(): Long
    }

    data class File(override val name: String, val size: Long) : Element {
        override fun calculateSize(): Long = size

    }

    data class Dir(override val name: String, val parent: Dir?, val children: MutableList<Element>, var size: Long? = null) : Element {
        override fun toString(): String = "Dir($name, children=$children)"

        override fun calculateSize(): Long {
            if (size != null) {
                return size as Long
            } else {
                val s = children.sumOf { it.calculateSize() }
                size = s
                return s
            }
        }
    }

    private fun part1(lines: List<String>): Any {
        val root = buildRoot(lines)

        var totalSize = 0L
        val stack = Stack<Dir>()
        stack.push(root)
        while (stack.isNotEmpty()) {
            val dir = stack.pop()
            val size = dir.calculateSize()
            if (size <= 100000) {
                totalSize += size
            }
            dir.children.forEach {
                if (it is Dir) {
                    stack.push(it)
                }
            }
        }

        return totalSize
    }

    private fun buildRoot(lines: List<String>): Dir {
        val root = Dir("/", null, emptyList<Element>().toMutableList())
        var curDir = root
        var i = 1
        while (i < lines.size) {
            val line = lines[i]
            if (line.startsWith("$ ls")) {
                ++i
                val readLines = lines.drop(i).takeWhile { !it.startsWith("$") }
                if (curDir.children.isEmpty()) {
                    readLines.forEach {
                        val parts = it.split(" ")
                        if (parts[0] == "dir") {
                            val dir = Dir(parts[1], curDir, emptyList<Element>().toMutableList())
                            curDir.children.add(dir)
                        } else {
                            val file = File(parts[1], parts[0].toLong())
                            curDir.children.add(file)
                        }
                    }
                }
                i += readLines.size
            } else if (line.startsWith("$ cd ")) {
                val dirName = line.split(" ")[2]
                if (dirName == "/") {
                    curDir = root
                } else if (dirName == "..") {
                    curDir = curDir.parent!!
                } else {
                    curDir = curDir.children.find { it.name == dirName } as Dir
                }
                ++i
            }
        }
        return root
    }

    private fun part2(lines: List<String>): Any {
        val root = buildRoot(lines)

        val totalCapability = 70000000
        val necessary = 30000000
        val weHave = root.calculateSize()

        val sizes = mutableListOf<Long>()
        val stack = Stack<Dir>()
        stack.push(root)
        while (stack.isNotEmpty()) {
            val dir = stack.pop()
            sizes.add(dir.calculateSize())
            dir.children.forEach {
                if (it is Dir) {
                    stack.push(it)
                }
            }
        }

        return sizes.sorted().reversed().last { totalCapability - weHave + it > necessary}
    }
}

