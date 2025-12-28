package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day04Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(int input, int expected) {
        var result = Day04.part1(input, input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(int input, int expected) {
        var result = Day04.part2(input, input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of(122345, 1),
                Arguments.of(111123, 1),
                Arguments.of(135679, 0),
                Arguments.of(111111, 1),
                Arguments.of(223450, 0),
                Arguments.of(123789, 0)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of(112233, 1),
                Arguments.of(123444, 0),
                Arguments.of(111122, 1)
        );
    }
}
