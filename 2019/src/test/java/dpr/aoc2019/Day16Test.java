package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day16Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, int phases, String expected) {
        var result = Day16.part1(input, phases);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, String expected) {
        var result = Day16.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("12345678", 4, "01029498"),
                Arguments.of("80871224585914546619083218645595", 100, "24176176"),
                Arguments.of("19617804207202209144916044189917", 100, "73745418"),
                Arguments.of("69317163492948606335995924319873", 100, "52432133")
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("03036732577212944063491565474664", "84462026"),
                Arguments.of("02935109699940807407585447034323", "78725270"),
                Arguments.of("03081770884921959731165446850517", "53553731")
        );
    }
}
