package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class Day21Test {

    @Test
    void testPart1Sanity() {
        var itemSets = Day21.generateItemSets();
        Object res = Day21.part1(itemSets);
        int val = ((Number) res).intValue();
        assertEquals(78, val);
    }
}
