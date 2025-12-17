package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.junit.jupiter.api.Test;

class Day10Test {

    @Test
    void testPart1And2() {
        var result = Day10.part1And2("1", List.of(1, 2, 3, 4, 5));
        assertEquals(List.of(2, 2, 4, 6, 6), result);
    }
}
