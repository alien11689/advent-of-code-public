package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day03Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(int input, int expected) {
        var result = Day03.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(int input, int expected) {
        var result = Day03.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of(1, 0),
                Arguments.of(12, 3),
                Arguments.of(23, 2),
                Arguments.of(1024, 31)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of(6, 10),
                Arguments.of(11, 23)
        );
    }
}
