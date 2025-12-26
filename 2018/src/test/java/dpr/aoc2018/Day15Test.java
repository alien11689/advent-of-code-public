
package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day15Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/15/" + filename);
        var result = Day15.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/15/" + filename);
        var result = Day15.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 27730),
                Arguments.of("test2.txt", 36334),
                Arguments.of("test3.txt", 39514),
                Arguments.of("test4.txt", 27755),
                Arguments.of("test5.txt", 28944),
                Arguments.of("test6.txt", 18740)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 4988),
                Arguments.of("test2.txt", 29064),
                Arguments.of("test3.txt", 31284),
                Arguments.of("test4.txt", 3478),
                Arguments.of("test5.txt", 6474),
                Arguments.of("test6.txt", 1140)
        );
    }
}
