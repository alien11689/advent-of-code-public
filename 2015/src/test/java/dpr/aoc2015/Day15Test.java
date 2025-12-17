package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day15Test {
    @Test
    void testPart1() {
        var lines = Util.getNotEmptyLinesFromFile("/15/test1.txt");
        var ingredients = Day15.parseIngredients(lines);
        int p1 = Day15.part1(ingredients);
        assertEquals(62842880, p1);
    }

    @Test
    void testPart2() {
        var lines = Util.getNotEmptyLinesFromFile("/15/test1.txt");
        var ingredients = Day15.parseIngredients(lines);
        int p2 = Day15.part2(ingredients);
        assertEquals(57600000, p2);
    }
}
