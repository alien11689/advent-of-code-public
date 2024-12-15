package dpr.aoc2024;

import dpr.commons.Dir;
import dpr.commons.Point2D;
import dpr.commons.Util;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;

class Day15 implements Day {
    public static void main(String... args) {
        new Day15().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test2.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 15;
    }

    private Object part1(List<String> lines) {
        Set<Point2D> blocks = new HashSet<>();
        Set<Point2D> boxes = new HashSet<>();
        Point2D robot = null;
        Queue<Dir> q = new LinkedList<>();

        for (int y = 0; y < lines.size(); y++) {
            String line = lines.get(y);
            if (line.startsWith("#")) {
                for (int x = 0; x < line.length(); x++) {
                    char c = line.charAt(x);
                    if (c == '#') {
                        blocks.add(new Point2D(x, y));
                    } else if (c == 'O') {
                        boxes.add(new Point2D(x, y));
                    } else if (c == '@') {
                        robot = new Point2D(x, y);
                    }
                }
            } else {
                for (char c : line.toCharArray()) {
                    q.offer(switch (c) {
                        case '>' -> Dir.E;
                        case '^' -> Dir.N;
                        case '<' -> Dir.W;
                        case 'v' -> Dir.S;
                        default -> throw new RuntimeException();
                    });
                }
            }
        }

        while (!q.isEmpty()) {
            Dir dir = q.poll();
            Point2D nextRobot = robot.move(dir, 1);
            if (blocks.contains(nextRobot)) {
                continue;
            }
            if (boxes.contains(nextRobot)) {
                Point2D placeForBox = nextRobot.move(dir, 1);
                while (true) {
                    if (blocks.contains(placeForBox)) {
                        break; // cannot move box
                    }
                    if (boxes.contains(placeForBox)) {
                        placeForBox = placeForBox.move(dir, 1);
                        continue; // checking next box in row
                    }
                    // empty space
                    robot = nextRobot;
                    if (!boxes.remove(nextRobot)) {
                        throw new RuntimeException();
                    }
                    boxes.add(placeForBox);
                    break;
                }
            } else { // empty
                robot = nextRobot;
            }
        }
        return boxes.stream().mapToInt(p -> p.getY() * 100 + p.getX()).sum();
    }

    private Object part2(List<String> lines) {
        return null;
    }
}
