package dpr.aoc2023

data class Point2D(val x: Int, val y: Int) {
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
}
