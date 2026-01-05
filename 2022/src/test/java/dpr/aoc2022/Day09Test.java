package dpr.aoc2022;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day09Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/09/test1.txt");
        var result = Day09.part1(input);
        assertEquals(13, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/09/" + filename);
        var result = Day09.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 1),
                Arguments.of("test2.txt", 36)
        );
    }
}
