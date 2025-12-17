package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day20Test {

    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(int input, int expected) {
        var res = Day20.part1(input);
        assertEquals(expected, res);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of(30, 2),
                Arguments.of(40, 3),
                Arguments.of(70, 4),
                Arguments.of(120, 6),
                Arguments.of(150, 8)
        );
    }
}
