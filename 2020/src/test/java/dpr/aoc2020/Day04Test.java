package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day04Test {
    @Test
    void testPart1() {
        var input = Util.getLinesFromFile("/04/test1.txt");
        var result = Day04.part1(input);
        assertEquals(2, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String fileName, int expected) {
        var input = Util.getLinesFromFile("/04/" + fileName);
        var result = Day04.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 2),
                Arguments.of("test2.txt", 0),
                Arguments.of("test3.txt", 4)
        );
    }
}
