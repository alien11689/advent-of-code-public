
package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day01Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, int expected) {
        var result = Day01.part1(Arrays.stream(input.split(", ")).toList());
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, int expected) {
        var result = Day01.part2(Arrays.stream(input.split(", ")).toList());
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("+1, -2, +3, +1", 3),
                Arguments.of("+1, +1, +1", 3),
                Arguments.of("+1, +1, -2", 0),
                Arguments.of("-1, -2, -3", -6)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("+1, -2, +3, +1", 2),
                Arguments.of("+1, -1", 0),
                Arguments.of("+3, +3, +4, -2, -4", 10),
                Arguments.of("-6, +3, +8, +5, -6", 5),
                Arguments.of("+7, +7, -2, -7, -4", 14)
        );
    }
}
