package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day09Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1And2(String input, int expectedPart1, int expectedPart2) {
        var result = Day09.part1And2(input);
        assertEquals(2, result.size());
        assertEquals(expectedPart1, result.get(0));
        assertEquals(expectedPart2, result.get(1));
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("{}", 1, 0),
                Arguments.of("{{{}}}", 6, 0),
                Arguments.of("{{},{}}", 5, 0),
                Arguments.of("{{{},{},{{}}}}", 16, 0),
                Arguments.of("{<a>,<a>,<a>,<a>}", 1, 4),
                Arguments.of("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9, 8),
                Arguments.of("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9, 0),
                Arguments.of("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3, 17),
                Arguments.of("<>", 0, 0),
                Arguments.of("<random characters>", 0, 17),
                Arguments.of("<<<<>", 0, 3),
                Arguments.of("<{!>}>", 0, 2),
                Arguments.of("<!!>", 0, 0),
                Arguments.of("<!!!>>", 0, 0),
                Arguments.of("<{o\"i!a,<{i<a>", 0, 10)
        );
    }
}
