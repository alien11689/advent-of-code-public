package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day22Test {

    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(Day22.State input, int expected) {
        var res = Day22.part1(input);
        assertEquals(expected, res);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of(Day22.initState(10, 250, new Day22.Boss(13, 8)), 226),
                Arguments.of(Day22.initState(10, 250, new Day22.Boss(14, 8)), 641)
        );
    }
}
