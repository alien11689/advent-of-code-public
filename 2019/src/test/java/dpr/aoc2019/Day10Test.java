package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day10Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/10/" + filename);
        var result = Day10.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String filename, int target, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/10/" + filename);
        var result = Day10.part2(input, target);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 8),
                Arguments.of("test2.txt", 33),
                Arguments.of("test3.txt", 35),
                Arguments.of("test4.txt", 41),
                Arguments.of("test5.txt", 210)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test5.txt", 1, 1112),
                Arguments.of("test5.txt", 2, 1201),
                Arguments.of("test5.txt", 3, 1202),
                Arguments.of("test5.txt", 10, 1208),
                Arguments.of("test5.txt", 20, 1600),
                Arguments.of("test5.txt", 50, 1609),
                Arguments.of("test5.txt", 100, 1016),
                Arguments.of("test5.txt", 199, 906),
                Arguments.of("test5.txt", 200, 802),
                Arguments.of("test5.txt", 201, 1009),
                Arguments.of("test6.txt", 1, 801),
                Arguments.of("test6.txt", 5, 902),
                Arguments.of("test6.txt", 9, 1501)
        );
    }
}
