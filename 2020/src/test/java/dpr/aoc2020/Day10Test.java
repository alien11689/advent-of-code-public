package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day10Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String fileName, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/10/" + fileName);
        var result = Day10.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String fileName, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/10/" + fileName);
        var result = Day10.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 5 * 7),
                Arguments.of("test2.txt", 22 * 10)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 8),
                Arguments.of("test2.txt", 19208)
        );
    }
}
