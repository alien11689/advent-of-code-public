package dpr.aoc2023

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/06/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/06/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val times = lines[0].split(":")[1].trim().split(Regex("\\s+")).map { it.toInt() }
        val distances = lines[1].split(":")[1].trim().split(Regex("\\s+")).map { it.toInt() }
        println(times)
        println(distances)
        var i = 0
        var wonsProduct = 1L
        while (i < times.size) {
            val sail = sail(times[i], distances[i])
            println("For ${times[i]} we can won in $sail")
            wonsProduct *= sail
            ++i
        }
        return wonsProduct
    }

    private fun sail(time: Int, distance: Int): Int {
        return (1..time).count { speed ->
            speed * (time - speed) > distance
        }
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

