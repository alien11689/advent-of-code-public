package dpr.aoc2025;

import java.util.List;

import dpr.commons.Day;
import dpr.commons.Util;

class Day01 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    @Override
    public int dayNum() {
        return 1;
    }

    int part1(List<String> lines) {
        return calculateZeroOccurrences(lines, false);
    }

    int part2(List<String> lines) {
        return calculateZeroOccurrences(lines, true);
    }

    private static int calculateZeroOccurrences(List<String> lines, boolean countPassing) {
        int pos = 50;
        int count = 0;
        int limit = 100;
        for (String line : lines) {
            var dir = line.charAt(0);
            var move = Integer.parseInt(line.substring(1));
            if (countPassing) {
                count += move / limit;
            }
            move %= limit;
            if (dir == 'L') {
                int intermediate = pos - move;
                if (countPassing && pos != 0 && intermediate < 0) {
                    ++count;
                }
                pos = (pos - move + limit) % limit;
            } else {
                int intermediate = pos + move;
                if (countPassing && intermediate > limit) {
                    ++count;
                }
                pos = (pos + move + limit) % limit;
            }
            if (pos == 0) {
                ++count;
            }
        }
        return count;
    }

    public static void main(String[] args) {
        new Day01().execute();
    }
}
