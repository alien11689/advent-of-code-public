package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day14Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/14/" + filename);
        var result = Day14.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String filename, long expected) {
        var input = Util.getNotEmptyLinesFromFile("/14/" + filename);
        var result = Day14.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 31),
                Arguments.of("test2.txt", 165),
                Arguments.of("test3.txt", 13312),
                Arguments.of("test4.txt", 180697),
                Arguments.of("test5.txt", 2210736)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test3.txt", 82892753),
                Arguments.of("test4.txt", 5586022),
                Arguments.of("test5.txt", 460664)
        );
    }
}
