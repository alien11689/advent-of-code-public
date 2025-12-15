package dpr.aoc2015

import dpr.commons.Util
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.util.stream.Stream

class Day02Test {

    @ParameterizedTest
    @MethodSource("part1Parameters")
    fun testPart1(input: List<String>, expected: Int) {
        val result = Day02.part1(Day02.readInput(input))
        assertEquals(expected, result)
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    fun testPart2(input: List<String>, expected: Int) {
        val result = Day02.part2(Day02.readInput(input))
        assertEquals(expected, result)
    }

    companion object {
        @JvmStatic
        fun part1Parameters(): Stream<Arguments> =
            Stream.of(
                Arguments.of(listOf("2x3x4"), 58),
                Arguments.of(listOf("1x1x10"), 43),
            )

        @JvmStatic
        fun part2Parameters(): Stream<Arguments> =
            Stream.of(
                Arguments.of(listOf("2x3x4"), 34),
                Arguments.of(listOf("1x1x10"), 14),
            )
    }
}