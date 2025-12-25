package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day11Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, int expected) {
        var result = Day11.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, int expected) {
        var result = Day11.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("ne,ne,ne", 3),
                Arguments.of("ne,ne,sw,sw", 0),
                Arguments.of("ne,ne,s,s", 2),
                Arguments.of("se,sw,se,sw,sw", 3)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("ne,ne,ne", 3),
                Arguments.of("ne,ne,sw,sw", 2),
                Arguments.of("ne,ne,s,s", 2),
                Arguments.of("se,sw,se,sw,sw", 3)
        );
    }
}
