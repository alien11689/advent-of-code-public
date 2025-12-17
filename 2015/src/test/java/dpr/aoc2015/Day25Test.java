package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day25Test {

    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(int column, int row, long expected) {
        var res = Day25.part1(column, row);
        assertEquals(expected, res);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of(1, 1, 20151125),
                Arguments.of(2, 3, 8057251),
                Arguments.of(6, 6, 27995004)
        );
    }
}
