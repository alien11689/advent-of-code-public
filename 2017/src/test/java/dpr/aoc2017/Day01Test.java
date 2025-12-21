package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day01Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, int expected) {
        var result = Day01.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, int expected) {
        var result = Day01.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("1122", 3),
                Arguments.of("1111", 4),
                Arguments.of("1234", 0),
                Arguments.of("91212129", 9)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("1212", 6),
                Arguments.of("1221", 0),
                Arguments.of("123425", 4),
                Arguments.of("123123", 12),
                Arguments.of("12131415", 4)
        );
    }
}
