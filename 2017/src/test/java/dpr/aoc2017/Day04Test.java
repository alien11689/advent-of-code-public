package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day04Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, int expected) {
        var result = Day04.part1(List.of(input));
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, int expected) {
        var result = Day04.part2(List.of(input));
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("aa bb cc dd ee", 1),
                Arguments.of("aa bb cc dd aa", 0),
                Arguments.of("aa bb cc dd aaa", 1)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("abcde fghij", 1),
                Arguments.of("abcde xyz ecdab", 0),
                Arguments.of("a ab abc abd abf abj", 1),
                Arguments.of("iiii oiii ooii oooi oooo", 1),
                Arguments.of("oiii ioii iioi iiio", 0)
        );
    }
}
