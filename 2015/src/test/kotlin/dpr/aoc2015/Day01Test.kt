package dpr.aoc2015

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.util.stream.Stream

class Day01Test {

    @ParameterizedTest
    @MethodSource("part1Parameters")
    fun testPart1(input: String, expected: Int) {
        val result = Day01.part1(input)
        assertEquals(expected, result)
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    fun testPart2(input: String, expected: Int) {
        val result = Day01.part2(input)
        assertEquals(expected, result)
    }

    companion object {
        @JvmStatic
        fun part1Parameters(): Stream<Arguments> =
            Stream.of(
                Arguments.of("(())", 0),
                Arguments.of("()()", 0),
                Arguments.of("(((", 3),
                Arguments.of("(()(()(", 3),
                Arguments.of("))(((((", 3),
                Arguments.of("())", -1),
                Arguments.of("))(", -1),
                Arguments.of(")))", -3),
                Arguments.of(")())())", -3),
            )

        @JvmStatic
        fun part2Parameters(): Stream<Arguments> =
            Stream.of(
                Arguments.of(")", 1),
                Arguments.of("()())", 5),
            )
    }
}