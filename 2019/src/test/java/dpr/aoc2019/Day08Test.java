package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day08Test {
    @Test
    void testPart1() {
        var result = Day08.part1("123456789012", 2, 2);
        assertEquals(1, result);
    }

    @Test
    void testPart2() {
        var result = Day08.part2("0222112222120000", 2, 2);
        assertEquals(" X\nX ", result);
    }
}
