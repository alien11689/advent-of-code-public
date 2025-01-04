package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Util;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

class Day01 implements Day {
    public static void main(String... args) {
        new Day01().execute();
    }

    @Override
    public void execute() {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
            Locations result = getLocations(lines);
            System.out.println(part1(result));
            System.out.println(part2(result));
        });
    }

    @Override
    public int dayNum() {
        return 1;
    }

    record Locations(List<Integer> lefts, List<Integer> rights) {
    }

    int part1(Locations result) {
        Collections.sort(result.lefts());
        Collections.sort(result.rights());
        var res = 0;
        for (int i = 0; i < result.lefts().size(); i++) {
            res += Math.abs(result.lefts().get(i) - result.rights().get(i));
        }
        return res;
    }

    @NotNull
    Locations getLocations(List<String> lines) {
        var lefts = new ArrayList<Integer>();
        var rights = new ArrayList<Integer>();
        lines.forEach(line -> {
            var parts = line.split(" +");
            int left = Integer.parseInt(parts[0]);
            int right = Integer.parseInt(parts[1]);
            lefts.add(left);
            rights.add(right);
        });
        return new Locations(lefts, rights);
    }

    long part2(Locations result) {
        var res = 0L;
        var mem = new HashMap<Integer, Long>();
        for (int left : result.lefts) {
            mem.computeIfAbsent(left, l -> result.rights.stream().filter(r -> r.equals(l)).count());
            res += left * mem.get(left);
        }
        return res;
    }
}
