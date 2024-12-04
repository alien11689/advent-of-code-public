package dpr.commons

import kotlin.math.abs
import kotlin.math.absoluteValue

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

    fun leftDown(): Point2D = copy(x = x - 1, y = y + 1)
    fun rightDown(): Point2D = copy(x = x + 1, y = y + 1)
    fun downModulo(size: Int): Point2D = Point2D(x, (y + 1) % size)
    fun rightModulo(size: Int): Point2D = Point2D((x + 1) % size, y)

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

    fun neighboursCross(): List<Point2D> = listOf(up(), down(), left(), right())

    fun neighboursDiag(): List<Point2D> {
        return listOf(
            Point2D(x = x + 1, y = y + 1),
            Point2D(x = x - 1, y = y + 1),
            Point2D(x = x + 1, y = y - 1),
            Point2D(x = x - 1, y = y - 1),
        )
    }

    fun mod(xSize: Int, ySize: Int): Point2D = copy(
        x = if (x < 0) (x + (x.absoluteValue / xSize + 1) * xSize) % xSize else x % xSize,
        y = if (y < 0) (y + (y.absoluteValue / ySize + 1) * ySize) % ySize else y % ySize,
    )

    fun move(dx: Int, dy: Int): Point2D = copy(x = x + dx, y = y + dy)
}
