package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day19Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String fileName, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/19/" + fileName);
        var result = Day19.part1(input);
        assertEquals(expected, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/19/test3.txt");
        var result = Day19.part2(input);
        assertEquals(12, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 2),
                Arguments.of("test2.txt", 3)
        );
    }
}
