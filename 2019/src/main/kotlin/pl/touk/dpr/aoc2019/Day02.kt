package pl.touk.dpr.aoc2019

import pl.touk.dpr.aoc2019.intcode.IntCodeComputer
import pl.touk.dpr.aoc2019.intcode.IntCodeComputerState
import java.lang.RuntimeException

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/02/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val v = IntCodeComputer.parseInput(input)
        v.put(1L, 12L)
        v.put(2L, 2L)
        IntCodeComputer.program(IntCodeComputerState(v))
        return v.get(0L)!!
    }

    private fun part2(input: String): Any {
        for(i in 0 until 100){
            for (j in 0 until 100){
                val v = IntCodeComputer.parseInput(input)
                v.put(1L, i.toLong())
                v.put(2L, j.toLong())
                IntCodeComputer.program(IntCodeComputerState(v))
                val res = v.getOrDefault(0L,0L)
                if(res == 19690720L){
                    return i*100 + j
                }
            }
        }
        throw RuntimeException()
    }
}
