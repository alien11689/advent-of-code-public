package dpr.commons

data class Point3D(val x: Int, val y: Int, val z: Int) {
    fun down(): Point3D = copy(z = z - 1)

    fun neighboursCross(): Set<Point3D> = setOf(
        copy(x = x - 1),
        copy(x = x + 1),
        copy(y = y - 1),
        copy(y = y + 1),
        copy(z = z - 1),
        copy(z = z + 1),
    )
}
