package dpr.commons

import kotlin.math.abs

data class Point2D(val x: Int, val y: Int) : Comparable<Point2D> {
    fun adjacentPoints(): Set<Point2D> = setOf(
        Point2D(x - 1, y - 1),
        Point2D(x + 1, y - 1),
        Point2D(x - 1, y + 1),
        Point2D(x + 1, y + 1),
        Point2D(x - 1, y),
        Point2D(x + 1, y),
        Point2D(x, y - 1),
        Point2D(x, y + 1),
    )

    fun up(steps: Int = 1): Point2D = this.copy(y = y - steps)
    fun down(steps: Int = 1): Point2D = this.copy(y = y + steps)
    fun left(steps: Int = 1): Point2D = this.copy(x = x - steps)
    fun right(steps: Int = 1): Point2D = this.copy(x = x + steps)
    fun manhattan(other: Point2D): Int = abs(x - other.x) + abs(y - other.y)
    fun move(dir: Dir, steps: Int = 1): Point2D = when (dir) {
        Dir.N -> up(steps)
        Dir.W -> left(steps)
        Dir.S -> down(steps)
        Dir.E -> right(steps)
    }

    override fun compareTo(other: Point2D): Int {
        val yComparison = y.compareTo(other.y)
        return if (yComparison == 0) {
            x.compareTo(other.x)
        } else {
            yComparison
        }
    }
}
