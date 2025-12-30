package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day12Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String filename, int steps, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/12/" + filename);
        var result = Day12.part1(input, steps);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String filename, long expected) {
        var input = Util.getNotEmptyLinesFromFile("/12/" + filename);
        var result = Day12.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 10, 179),
                Arguments.of("test2.txt", 100, 1940)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 2772),
                Arguments.of("test2.txt", 4686774924L)
        );
    }
}
