package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day18Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/18/" + filename);
        var result = Day18.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String filename, long expected) {
        var input = Util.getNotEmptyLinesFromFile("/18/" + filename);
        var result = Day18.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 8),
                Arguments.of("test2.txt", 86),
                Arguments.of("test3.txt", 132),
                Arguments.of("test4.txt", 136),
                Arguments.of("test5.txt", 81)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test6.txt", 8),
                Arguments.of("test7.txt", 24),
                Arguments.of("test8.txt", 32),
                Arguments.of("test9.txt", 72)
        );
    }
}
