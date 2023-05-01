package dpr.eulerproject

object Problem0028 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        var maxRight = 0
        var maxLeft = 0
        var maxUp = 0
        var maxDown = 0
        var cur = 0 to 0
        var sumDiagonal = 0L
        var value = 1L
        var dir = Dir.RIGHT

        val limit = 1001 * 1001
//        val limit = 5*5

        while (value <= limit) {
            when (dir) {
                Dir.RIGHT -> {
                    val x = cur.first
                    cur = (++maxRight) to cur.second
                    value += cur.first - x
                    dir = Dir.DOWN
                    sumDiagonal += value - 1
                }

                Dir.LEFT -> {
                    val x = cur.first
                    cur = (--maxLeft) to cur.second
                    value += x - cur.first
                    dir = Dir.UP
                    sumDiagonal += value
                }

                Dir.UP -> {
                    val y = cur.second
                    cur = cur.first to (--maxUp)
                    value += y - cur.second
                    dir = Dir.RIGHT
                    sumDiagonal += value
                }

                Dir.DOWN -> {
                    val y = cur.second
                    cur = cur.first to (++maxDown)
                    value += cur.second - y
                    dir = Dir.LEFT
                    sumDiagonal += value
                }
            }
        }
        println(sumDiagonal)

    }

    enum class Dir {
        RIGHT,
        DOWN,
        LEFT,
        UP
    }
}
