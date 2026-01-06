package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.Map;

import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;

import dpr.commons.Util;
import kotlin.Pair;

class Day19Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/19/test1.txt");
        var splitInput = Day19.parseInput(input);
        var result = Day19.part1(splitInput.component1(), splitInput.component2());
        assertEquals(19114, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/19/test1.txt");
        var splitInput = Day19.parseInput(input);
        var result = Day19.part2(splitInput.component1());
        assertEquals(167409079868000L, result);
    }
}
