
package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.junit.jupiter.api.Test;

class Day15Test {
    @Test
    void testPart1And2() {
        var result = Day15.part1And2(List.of(
                new Day15.Disk(5, 4, 1),
                new Day15.Disk(2, 1, 2)
        ));
        assertEquals(5, result);
    }
}
