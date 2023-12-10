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

    fun up(): Point2D = this.copy(y = y - 1)
    fun down(): Point2D = this.copy(y = y + 1)
    fun left(): Point2D = this.copy(x = x - 1)
    fun right(): Point2D = this.copy(x = x + 1)
}
