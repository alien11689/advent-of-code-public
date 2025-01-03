package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Util;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

class Day09 implements Day {
    public static void main(String... args) {
        new Day09().execute();
    }

    @Override
    public void execute() {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 9;
    }

    record Elem(int fileId, int count, boolean empty) {
    }

    private Object part1(List<String> lines) {
        LinkedList<Elem> elems = parseElements(lines);
//        elems.forEach(System.out::println);
        long checksum = 0;
        long position = 0;
        while (!elems.isEmpty()) {
            Elem elem = elems.removeFirst();
            if (elem.empty) {
                Elem last = elems.removeLast();
                if (last.empty) {
                    elems.addFirst(elem);
                } else if (last.count > elem.count) {
                    elems.addFirst(new Elem(last.fileId, elem.count, false));
                    elems.addLast(new Elem(last.fileId, last.count - elem.count, false));
                } else if (last.count == elem.count) {
                    elems.addFirst(new Elem(last.fileId, last.count, false));
                } else {
                    elems.addFirst(new Elem(0, elem.count - last.count, true));
                    elems.addFirst(new Elem(last.fileId, last.count, false));
                }
            } else {
                for (int i = 0; i < elem.count; i++) {
                    checksum += elem.fileId * (position++);
//                    System.out.print(elem.fileId);
                }
            }
//            System.out.println(elems);
        }
//        System.out.println();
        return checksum;
    }

    @NotNull
    private static LinkedList<Elem> parseElements(List<String> lines) {
        char[] line = lines.getFirst().toCharArray();
        LinkedList<Elem> elems = new LinkedList<>();
        for (int i = 0; i < line.length; i++) {
            int count = line[i] - '0';
            boolean empty = i % 2 == 1;
            if (count != 0) {
                elems.add(new Elem(empty ? (i / 2 - 1) : (i / 2), count, empty));
            }
        }
        return elems;
    }

    private Object part2(List<String> lines) {
        LinkedList<Elem> initial = parseElements(lines);
        List<Elem> elems = new ArrayList<>(initial);
        int currentFileId = elems.stream().mapToInt(e -> e.fileId).max().getAsInt();
        int lowestFileId = elems.stream().mapToInt(e -> e.fileId).min().getAsInt();
        while (currentFileId > 0) {
            int finalCurrentFileId = currentFileId;
            Elem current = elems.stream().filter(e -> e.fileId == finalCurrentFileId).findFirst().get();
            int currentIdx = elems.indexOf(current);
            for (int i = 0; i < currentIdx; ++i) {
                Elem checked = elems.get(i);
                if (checked.empty && checked.count >= current.count) {
                    // remove current
                    elems.set(currentIdx, new Elem(--lowestFileId, current.count, true));
                    // place current in new place
                    elems.set(i, current);
                    if (checked.count > current.count) {
                        elems.add(i + 1, new Elem(checked.fileId, checked.count - current.count, true));
                    }
                    break;
                }
            }
            --currentFileId;
        }
        long checksum = 0;
        long position = 0;
        for (Elem elem : elems) {
            for (int i = 0; i < elem.count; i++) {
                if (elem.empty) {
//                    System.out.print('.');
                    ++position;
                } else {
                    checksum += elem.fileId * (position++);
//                    System.out.print(elem.fileId);
                }
            }
        }
//        System.out.println();
        return checksum;
    }
}
