package dpr.aoc2015;

import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day02Test {

    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(List<String> input, int expected) {
        Object result = Day02.part1(Day02.readInput(input));
        Assertions.assertEquals(expected, ((Number) result).intValue());
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(List<String> input, int expected) {
        Object result = Day02.part2(Day02.readInput(input));
        Assertions.assertEquals(expected, ((Number) result).intValue());
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of(List.of("2x3x4"), 58),
                Arguments.of(List.of("1x1x10"), 43)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of(List.of("2x3x4"), 34),
                Arguments.of(List.of("1x1x10"), 14)
        );
    }
}

