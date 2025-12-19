
package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Test;

class Day11Test {
    @Test
    void testPart1() {
        var result = Day11.part1(List.of(Set.of(-1, -2), Set.of(1), Set.of(2), Set.of()));
        assertEquals(11, result);
    }
}
