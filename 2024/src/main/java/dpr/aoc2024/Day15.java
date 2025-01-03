package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Dir;
import dpr.commons.Point2D;
import dpr.commons.Util;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class Day15 implements Day {
    public static void main(String... args) {
        new Day15().execute();
    }

    @Override
    public void execute() {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test2.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test3.txt", dayNum()));
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

    record Box(Point2D left, Point2D right) {
        public boolean contains(Point2D p) {
            return p.equals(left) || p.equals(right);
        }

        public Box move(Dir dir) {
            return new Box(left.move(dir, 1), right.move(dir, 1));
        }
    }

    private Object part2(List<String> lines) {
        Set<Point2D> blocks = new HashSet<>();
        Set<Box> boxes = new HashSet<>();
        Point2D robot = null;
        Queue<Dir> q = new LinkedList<>();

        for (int y = 0; y < lines.size(); y++) {
            String line = lines.get(y);
            if (line.startsWith("#")) {
                for (int x = 0; x < line.length(); x++) {
                    char c = line.charAt(x);
                    if (c == '#') {
                        blocks.add(new Point2D(x * 2, y));
                        blocks.add(new Point2D(x * 2 + 1, y));
                    } else if (c == 'O') {
                        boxes.add(new Box(new Point2D(x * 2, y), new Point2D(x * 2 + 1, y)));
                    } else if (c == '@') {
                        robot = new Point2D(x * 2, y);
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
            Set<Point2D> boxesFlatten = boxes.stream().flatMap(b -> Stream.of(b.left, b.right)).collect(Collectors.toSet());
            if (boxesFlatten.contains(nextRobot)) { // box to move
                Set<Box> boxesCopy = new HashSet<>(boxes);
                Set<Box> boxesToMove = boxes.stream().filter(b -> b.contains(nextRobot)).collect(Collectors.toSet());
                boxesCopy.removeAll(boxesToMove);
                while (true) {
                    Set<Box> newBoxes = boxesToMove.stream().map(b -> b.move(dir)).collect(Collectors.toSet());
                    if (newBoxes.stream().anyMatch(b -> blocks.contains(b.left) || blocks.contains(b.right))) {
                        //cannot move because of blocks
                        break;
                    }
                    Set<Point2D> newBoxesFlattenInternal = newBoxes.stream().flatMap(b -> Stream.of(b.left, b.right)).collect(Collectors.toSet());
                    Set<Box> nextBoxesToMove = boxesCopy.stream().filter(b -> newBoxesFlattenInternal.contains(b.left) || newBoxesFlattenInternal.contains(b.right)).collect(Collectors.toSet());
                    if (nextBoxesToMove.isEmpty()) {
                        // can move robot and all the boxes
                        robot = nextRobot;
//                        System.out.println("Removing " + boxesToMove);
//                        System.out.println("Adding " + newBoxes);
                        boxes.removeAll(boxesToMove);
                        boxes.addAll(newBoxes);
                        break;
                    } else {
                        // add next boxes and try to move all again
                        boxesCopy.removeAll(nextBoxesToMove);
                        boxesToMove.addAll(nextBoxesToMove);
                    }
                }
            } else { // empty
//                System.out.println("Only moving robot");
                robot = nextRobot;
            }
//            draw(dir, robot, blocks, boxes);
        }
        return boxes.stream().mapToInt(p -> p.left.getY() * 100 + p.left.getX()).sum();
    }

    private void draw(Dir dir, Point2D robot, Set<Point2D> blocks, Set<Box> boxes) {
        System.out.println(dir);
        Point2D limit = blocks.stream().sorted().toList().getLast();
        Set<Point2D> lefts = boxes.stream().map(Box::left).collect(Collectors.toSet());
        Set<Point2D> rights = boxes.stream().map(Box::right).collect(Collectors.toSet());
        for (int y = 0; y <= limit.getY(); ++y) {
            for (int x = 0; x <= limit.getX(); ++x) {
                Point2D cur = new Point2D(x, y);
                if (blocks.contains(cur)) {
                    System.out.print('#');
                } else if (lefts.contains(cur)) {
                    System.out.print('[');
                } else if (rights.contains(cur)) {
                    System.out.print(']');
                } else if (robot.equals(cur)) {
                    System.out.print('@');
                } else {
                    System.out.print('.');
                }
            }
            System.out.println();
        }
    }
}
