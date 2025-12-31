package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day20Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/20/" + filename);
        var result = Day20.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String filename, long expected) {
        var input = Util.getNotEmptyLinesFromFile("/20/" + filename);
        var result = Day20.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 23),
                Arguments.of("test2.txt", 58)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test3.txt", 396)
        );
    }
}
