package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day07Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/07/test1.txt");
        var result = Day07.part1(input);
        assertEquals(4, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String fileName, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/07/" + fileName);
        var result = Day07.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 32),
                Arguments.of("test2.txt", 126)
        );
    }
}
