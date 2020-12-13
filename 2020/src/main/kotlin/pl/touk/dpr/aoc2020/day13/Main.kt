package pl.touk.dpr.aoc2020.day13

import pl.touk.dpr.aoc2020.Util
import java.math.BigInteger

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
//        val input = Util.getNotEmptyLinesFromFile("/12/test.txt")
        val input = Util.getNotEmptyLinesFromFile("/13/input.txt")
        println(part1(input))
//        test(part2("7,13,x,x,59,x,31,19") == 1068781L)
//        test(part2("17,x,13,19") == 3417L)
//        test(part2("67,7,59,61") == 754018L)
//        test(part2("67,x,7,59,61") == 779210L)
//        test(part2("67,7,x,59,61") == 1261476L)
//        test(part2("1789,37,47,1889") == 1202161486L)
        println(part2(input[1]))
    }

    private fun part1(input: List<String>): Any {
        val timestamp = input[0].toLong()
        val buses = input[1].split(',').filter { it != "x" }.map { it.toLong() }
        var i = 0
        while (true) {
            val ts = timestamp + i
            val bus = buses.find { ts % it == 0L }
            if (bus != null) {
                return i * bus
            }
            ++i
        }
    }

    private fun part2(input: String): Long {
        val buses = input.split(",").toList()
        val busToMinut = mutableMapOf<Long, Int>()
        buses.forEachIndexed { index, s ->
            if (s != "x") {
                busToMinut[s.toLong()] = index
            }
        }
        val maxBus = busToMinut.keys.max()!!
        val maxBusShift = busToMinut[maxBus]!!.toBigInteger()
        busToMinut.remove(maxBus)

//        val incr = BigInteger.valueOf(maxBus * busToMinut.keys.fold(1L) { acc, l -> acc * l })
        val incr = BigInteger.valueOf(maxBus)
        var cur: BigInteger = 100000000000000L.toBigInteger() / BigInteger.valueOf(maxBus)
        cur *= BigInteger.valueOf(maxBus)
        cur =  maxBusShift
        var res = listOf<BigInteger>()
        while (res.size < 50) {
            println("Checking $cur: $res")
            if (busToMinut.filter { it.key in setOf(397L) }.all { entry ->
                        (cur - maxBusShift + entry.value.toBigInteger()) % entry.key.toBigInteger() == BigInteger.ZERO
                    }) {
                res = res + (cur - maxBusShift)
            }
            cur += incr
        }
        // 937 397 -> [181778, 553767, 925756, 1297745, 1669734, 2041723, 2413712, 2785701, 3157690, 3529679, 3901668, 4273657, 4645646, 5017635, 5389624, 5761613, 6133602, 6505591, 6877580, 7249569, 7621558, 7993547, 8365536, 8737525, 9109514, 9481503, 9853492, 10225481, 10597470, 10969459, 11341448, 11713437, 12085426, 12457415, 12829404, 13201393, 13573382, 13945371, 14317360, 14689349, 15061338, 15433327, 15805316, 16177305, 16549294, 16921283, 17293272, 17665261, 18037250, 18409239]
        // 937 397 41 -> [3157690, 18409239, 33660788, 48912337, 64163886, 79415435, 94666984, 109918533, 125170082, 140421631, 155673180, 170924729, 186176278, 201427827, 216679376, 231930925, 247182474, 262434023, 277685572, 292937121, 308188670, 323440219, 338691768, 353943317, 369194866, 384446415, 399697964, 414949513, 430201062, 445452611]
        // 937 397 41 37 -> [323440219, 887747532, 1452054845, 2016362158, 2580669471, 3144976784, 3709284097, 4273591410, 4837898723, 5402206036]
        // 937 397 41 37 17 -> [4273591410]
        println(res)
        throw RuntimeException()
    }

    private fun partIterative(input: String): Long {
        val buses = input.split(",").toList()
        val busToMinut = mutableMapOf<Long, Int>()
        buses.forEachIndexed { index, s ->
            if (s != "x") {
                busToMinut[s.toLong()] = index
            }
        }
        val maxBus = busToMinut.keys.max()!!
        val maxBusShift = busToMinut[maxBus]!!
        busToMinut.remove(maxBus)

        var cur: Long = maxBus
        while (true) {
            println("Checking $cur")
            if (busToMinut.all { entry ->
                        (cur - maxBusShift + entry.value) % entry.key == 0L
                    }) {
                return cur - maxBusShift
            }
            cur += maxBus
        }
    }

    fun test(b: Boolean) {
        if (!b) {
            throw RuntimeException("Error")
        }
    }
}

