package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day03Test {

    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, int expected) {
        var result = Day03.part1(input);
        assertEquals(expected, ((Number) result).intValue());
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, int expected) {
        var result = Day03.part2(input);
        assertEquals(expected, ((Number) result).intValue());
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of(">", 2),
                Arguments.of("^>v<", 4),
                Arguments.of("^v^v^v^v^v", 2)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("^v", 3),
                Arguments.of("^>v<", 3),
                Arguments.of("^v^v^v^v^v", 11)
        );
    }
}

