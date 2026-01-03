package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day18Test {
    @ParameterizedTest
    @MethodSource("part1And2Parameters")
    void testPart1(String input, long expectedPart1, long expectedPart2) {
        var result = Day18.part1(List.of(input));
        assertEquals(expectedPart1, result);
    }

    @ParameterizedTest
    @MethodSource("part1And2Parameters")
    void testPart2(String input, long expectedPart1, long expectedPart2) {
        var result = Day18.part2(List.of(input));
        assertEquals(expectedPart2, result);
    }

    private static Stream<Arguments> part1And2Parameters() {
        return Stream.of(
                Arguments.of("1 + 2 * 3 + 4 * 5 + 6", 71, 231),
                Arguments.of("1 + (2 * 3) + (4 * (5 + 6))", 51, 51),
                Arguments.of("2 * 3 + (4 * 5)", 26, 46),
                Arguments.of("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437, 1445),
                Arguments.of("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240, 669060),
                Arguments.of("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632, 23340)
        );
    }
}
