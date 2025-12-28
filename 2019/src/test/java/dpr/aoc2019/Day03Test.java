package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day03Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(List<String> input, int expected) {
        var result = Day03.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(List<String> input, int expected) {
        var result = Day03.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of(List.of("R8,U5,L5,D3", "U7,R6,D4,L4"), 6),
                Arguments.of(List.of("R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83"), 159),
                Arguments.of(List.of("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51","U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"), 135)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of(List.of("R8,U5,L5,D3", "U7,R6,D4,L4"), 30),
                Arguments.of(List.of("R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83"), 610),
                Arguments.of(List.of("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51","U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"), 410)
        );
    }
}
