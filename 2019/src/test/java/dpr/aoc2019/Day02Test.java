package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day02Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, int expected) {
        var result = Day02.part1(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("1,9,10,3,2,3,11,0,99,30,40,50", 100),
                Arguments.of("1,0,0,0,99", 2),
                Arguments.of("2,3,0,3,99", 2),
                Arguments.of("2,4,4,5,99,0", 2),
                Arguments.of("1,1,1,4,99,5,6,0,99", 30)
        );
    }
}
