package dpr.aoc2023

import dpr.commons.Util

object All {
    @JvmStatic
    fun main(args: Array<String>) {
        if (args.isNotEmpty()) {
            val m = Class.forName("${All.javaClass.packageName}.Day${args[0]}")
                .getDeclaredMethod("main", Array<String>::class.java)
            println("Day${args[0]}")
            m.invoke(null, args)
            return
        }
        Util.measureTime {
            println("Day01")
            Day01.main(args)
            println("Day02")
            Day02.main(args)
            println("Day03")
            Day03.main(args)
            println("Day04")
            Day04.main(args)
            println("Day05")
            Day05.main(args)
            println("Day06")
            Day06.main(args)
            println("Day07")
            Day07.main(args)
            println("Day08")
            Day08.main(args)
            println("Day09")
            Day09.main(args)
            println("Day10")
            Day10.main(args)
            println("Day11")
            Day11.main(args)
            println("Day12")
            Day12.main(args)
            println("Day13")
            Day13.main(args)
            println("Day14")
            Day14.main(args)
            println("Day15")
            Day15.main(args)
            println("Day16")
            Day16.main(args)
            println("Day17")
            Day17.main(args)
            println("Day18")
            Day18.main(args)
            println("Day19")
            Day19.main(args)
            println("Day20")
            Day20.main(args)
            println("Day21")
            Day21.main(args)
            println("Day22")
            Day22.main(args)
            println("Day23")
            Day23.main(args)
            println("Day24")
            Day24.main(args)
            println("Day25")
            Day25.main(args)
        }
    }
}
