package pl.touk.dpr.synacorchallenge

import java.util.PriorityQueue

object Orb {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val cur = Point(0, 0)
        val maze = listOf(
// maze is upside down
                listOf("", "-", "9", "*"),
                listOf("+", "4", "-", "18"),
                listOf("4", "*", "11", "*"),
                listOf("*", "8", "-", "1"),
        )
        val pq = PriorityQueue<Path>()
        pq.offer(Path(listOf(cur), Operation.NONE, 22))
        while (pq.isNotEmpty()) {
            val path = pq.poll()
//            println("Current: $path, pq: ${pq.size}")
            path.route.last().neighbours().forEach { np ->
                val newPath: Path = when (val symbol = maze[np.y][np.x]) {
                    "*" -> path.add(np, Operation.TIMES)
                    "+" -> path.add(np, Operation.PLUS)
                    "-" -> path.add(np, Operation.MINUS)
                    else -> path.addAndResult(np, symbol.toInt())
                }
                if (newPath.route.last() == Point(3, 3)) {
                    if (newPath.orb == 30) {
                        println(newPath.route)
                        return@measureTime
                    }
                } else if (newPath.orb <= 0) {
                    // do nothing
                } else {
                    pq.offer(newPath)
                }
            }
        }
    }

    data class Path(val route: List<Point>, val operation: Operation, val orb: Int) : Comparable<Path> {
        override fun compareTo(other: Path): Int {
            if (route.size == other.route.size) {
                return orb - other.orb
            }
            return route.size - other.route.size
        }

        fun add(np: Point, op: Operation): Path =
                copy(route = route + np, operation = op)

        fun addAndResult(np: Point, score: Int): Path =
                copy(
                        route = route + np,
                        operation = Operation.NONE,
                        orb = when (operation) {
                            Operation.PLUS -> orb + score
                            Operation.MINUS -> orb - score
                            Operation.TIMES -> orb * score
                            Operation.NONE -> throw RuntimeException("Invalid path...")
                        })
    }

    data class Point(val x: Int, val y: Int) {
        fun neighbours(): Set<Point> =
                setOf(
                        Point(x + 1, y),
                        Point(x - 1, y),
                        Point(x, y + 1),
                        Point(x, y - 1),
                )
                        .filterNot { it.x == 0 && it.y == 0 }
                        .filter { it.x >= 0 && it.y >= 0 }
                        .filter { it.x <= 3 && it.y <= 3 }
                        .toSet()

    }

    enum class Operation {
        NONE,
        PLUS,
        MINUS,
        TIMES
    }
}
