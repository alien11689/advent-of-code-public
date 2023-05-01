package dpr.aoc2021

import java.util.PriorityQueue

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/12/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val connections = lines.map { it.split("-").toSet() }.toSet()
        val pq = initPQ()
        var fullPaths = 0
        while (!pq.isEmpty()) {
            val cur = pq.poll()
            if (cur.last == "end") {
                fullPaths += 1
//                println("Finished ${cur.path}")
                continue
            }
//            println("Checking $cur")
            for (n in getNeighbours(connections, cur)) {
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
        val pq = initPQ()
        var fullPaths = 0
        while (!pq.isEmpty()) {
            val cur = pq.poll()
            if (cur.last == "end") {
                fullPaths += 1
//                println("Finished ${cur.path}")
                continue
            }
//            println("Checking $cur")
            for (n in getNeighbours(connections, cur)) {
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

    private fun getNeighbours(connections: Set<Set<String>>, cur: Path) = connections.filter { cur.last in it }.map { (it - cur.last).first() }

    private fun initPQ(): PriorityQueue<Path> {
        val pq = PriorityQueue<Path>()
        pq.add(Path("start", listOf("start"), setOf("start")))
        return pq
    }
}

