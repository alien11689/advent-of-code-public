package dpr.aoc2022;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day05Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/05/test1.txt");
        var result = Day05.part1(input, test1Stack());
        assertEquals("CMZ", result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/05/test1.txt");
        var result = Day05.part2(input, test1Stack());
        assertEquals("MCD", result);
    }

    private static List<Stack<Character>> test1Stack() {
        var s1 = new Stack<Character>();
        s1.push('Z');
        s1.push('N');
        var s2 = new Stack<Character>();
        s2.push('M');
        s2.push('C');
        s2.push('D');
        var s3 = new Stack<Character>();
        s3.push('P');
        var l = new ArrayList<Stack<Character>>();
        l.add(s1);
        l.add(s2);
        l.add(s3);
        return l;
    }
}
