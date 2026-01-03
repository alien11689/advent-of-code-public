package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.math.BigInteger;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day13Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/13/test1.txt");
        var result = Day13.part1(input);
        assertEquals(295, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, long expected) {
        var result = Day13.part2(input);
        assertEquals(BigInteger.valueOf(expected), result);
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("7,13,x,x,59,x,31,19", 1068781L),
                Arguments.of("17,x,13,19", 3417L),
                Arguments.of("67,7,59,61", 754018L),
                Arguments.of("67,x,7,59,61", 779210L),
                Arguments.of("67,7,x,59,61", 1261476L),
                Arguments.of("1789,37,47,1889", 1202161486L)
        );
    }
}
