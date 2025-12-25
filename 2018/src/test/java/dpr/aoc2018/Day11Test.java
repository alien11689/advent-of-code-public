
package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day11Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(int input, String expected) {
        var result = Day11.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(int input, String expected) {
        var result = Day11.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of(18, "33,45"),
                Arguments.of(42, "21,61")
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of(18, "90,269,16"),
                Arguments.of(42, "232,251,12")
        );
    }
}
