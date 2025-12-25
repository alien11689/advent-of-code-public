
package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day09Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(int players, int maxScore, int expected) {
        var result = Day09.part1(players, maxScore);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of(10, 1618, 8317),
                Arguments.of(13, 7999, 146373),
                Arguments.of(17, 1104, 2764),
                Arguments.of(21, 6111, 54718),
                Arguments.of(30, 5807, 37305)
        );
    }
}
