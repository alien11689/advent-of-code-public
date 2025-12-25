package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day22Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(int iterations, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/22/test1.txt");
        var result = Day22.part1(input, iterations);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(int iterations, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/22/test1.txt");
        var result = Day22.part2(input, iterations);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of(70, 41),
                Arguments.of(10_000, 5587)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of(100, 26),
                Arguments.of(10_000_000, 2511944)
        );
    }
}
