package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.List;
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
        var result = Day01.part2(List.of(input));
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("12", 2),
                Arguments.of("14", 2),
                Arguments.of("1969", 654),
                Arguments.of("100756", 33583)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("14", 2),
                Arguments.of("1969", 966),
                Arguments.of("100756", 50346)
        );
    }
}
