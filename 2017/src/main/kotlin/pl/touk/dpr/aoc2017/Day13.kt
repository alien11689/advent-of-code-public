package pl.touk.dpr.aoc2017

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/13/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val pos2Range = readInput(input)

        var myPos = 0
        val caught = mutableSetOf<Int>()
        val max = pos2Range.keys.maxOrNull()!!

        while (myPos <= max) {
            val range = pos2Range[myPos]
            if (range == null) {
                ++myPos
                continue
            }
            if (countPos1(myPos, range) == 0) {
//                println( "$myPos/$max caught")
                caught.add(myPos)
            } else {
//                println "$myPos/$max not caught"
            }
            ++myPos
        }

        return caught.map { pos2Range[it]!! * it }.sum()
    }

    private fun readInput(input: List<String>): MutableMap<Int, Int> {
        val pos2Range = mutableMapOf<Int, Int>()

        input.forEach {
            val parts = it.split(':')
            val num = parts[0].trim().toInt()
            val range = parts[1].trim().toInt()

            pos2Range[num] = range

        }
        return pos2Range
    }

    private fun countPos1(current: Int, range: Int): Int {
        var down = true
        var start = 0
        var tick = 0
        while (tick < current) {
            // println "Laser $current is in pos $start/$range"
            start += if (down) 1 else -1
            if (start == range - 1) {
                down = false
            }
            if (start == 0) {
                down = true
            }
            ++tick
        }
        return start
    }

    private fun part2(input: List<String>): Any {
        val pos2Range = readInput(input)
        var delay = 0
        val max = pos2Range.keys.maxOrNull()!!
        while (true) {
            var myPos = 0
            val caught = mutableSetOf<Int>()
            ++delay
            while (myPos <= max) {
                val range = pos2Range[myPos]
                if (range == null) {
                    ++myPos
                    continue
                }
                if (countPos2(myPos, range, delay)) {
                    caught.add(myPos)
                    break
                }
                ++myPos
            }
            if (caught.isEmpty()) {
                return delay
            }
        }
    }

    private fun countPos2(current: Int, range: Int, delay: Int): Boolean {
        return (current + delay) % (2 * (range - 1)) == 0
    }
}