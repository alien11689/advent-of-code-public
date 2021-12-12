package pl.touk.dpr.aoc2021

import java.util.PriorityQueue

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/12/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val connections = lines.map { it.split("-").toSet() }.toSet()
        val pq = PriorityQueue<Path>()
        pq.add(Path("start", listOf("start"), setOf("start")))
        var fullPaths = 0
        while (!pq.isEmpty()) {
            val cur = pq.poll()
            if (cur.last == "end") {
                fullPaths += 1
//                println("FInished ${cur.path}")
                continue
            }
//            println("Checking $cur")
            val neighbours = connections.filter { cur.last in it }.map { (it - cur.last).first() }
            for (n in neighbours) {
                if (n == n.lowercase()) {
                    if (n !in cur.visitedCaves) {
                        pq.offer(Path(n, cur.path + n, cur.visitedCaves + n))
                    }
                } else {
                    pq.offer(Path(n, cur.path + n, cur.visitedCaves))
                }
            }
        }
        return fullPaths
    }

    data class Path(
        val last: String,
        val path: List<String>,
        val visitedCaves: Set<String>,
        val visitedCaves2: String? = null
    ) : Comparable<Path> {
        override fun compareTo(other: Path): Int {
            return path.size.compareTo(other.path.size)
        }

    }

    private fun part2(lines: List<String>): Any {
        val connections = lines.map { it.split("-").toSet() }.toSet()
        val pq = PriorityQueue<Path>()
        pq.add(Path("start", listOf("start"), setOf("start")))
        var fullPaths = 0
        while (!pq.isEmpty()) {
            val cur = pq.poll()
            if (cur.last == "end") {
                fullPaths += 1
//                println("Finished ${cur.path}")
                continue
            }
//            println("Checking $cur")
            val neighbours = connections.filter { cur.last in it }.map { (it - cur.last).first() }
            for (n in neighbours) {
                if (n == n.lowercase()) {
                    if (n == "start") {
                        continue
                    } else if (n !in cur.visitedCaves) {
                        pq.offer(Path(n, cur.path + n, cur.visitedCaves + n, cur.visitedCaves2))
                    } else if (cur.visitedCaves2 == null) {
                        pq.offer(Path(n, cur.path + n, cur.visitedCaves, n))
                    }
                } else {
                    pq.offer(Path(n, cur.path + n, cur.visitedCaves, cur.visitedCaves2))
                }
            }
        }
        return fullPaths
    }
}

