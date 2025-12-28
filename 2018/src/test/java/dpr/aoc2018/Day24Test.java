package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.junit.jupiter.api.Test;

class Day24Test {
    @Test
    void testPart1() {
        var input = List.of(
                new Day24.Group(Day24.Team.Immune, 1, 17, 5390, List.of("radiation", "bludgeoning"), List.of(), 2, 4507, "fire", null, null),
                new Day24.Group(Day24.Team.Immune, 2, 989, 1274, List.of("slashing", "bludgeoning"), List.of("fire"), 3, 25, "slashing", null, null),
                new Day24.Group(Day24.Team.Infection, 3, 801, 4706, List.of("radiation"), List.of(), 1, 116, "bludgeoning", null, null),
                new Day24.Group(Day24.Team.Infection, 4, 4485, 2961, List.of("fire", "cold"), List.of("radiation"), 4, 12, "slashing", null, null)
        );
        var result = Day24.part1(input);
        assertEquals(5216, result);
    }

    @Test
    void testPart2() {
        var input = List.of(
                new Day24.Group(Day24.Team.Immune, 1, 17, 5390, List.of("radiation", "bludgeoning"), List.of(), 2, 6077, "fire", null, null),
                new Day24.Group(Day24.Team.Immune, 2, 989, 1274, List.of("slashing", "bludgeoning"), List.of("fire"), 3, 1595, "slashing", null, null),
                new Day24.Group(Day24.Team.Infection, 3, 801, 4706, List.of("radiation"), List.of(), 1, 116, "bludgeoning", null, null),
                new Day24.Group(Day24.Team.Infection, 4, 4485, 2961, List.of("fire", "cold"), List.of("radiation"), 4, 12, "slashing", null, null)
        );
        var result = Day24.part2(input);
        assertEquals(51, result);
    }
}
