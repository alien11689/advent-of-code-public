import groovy.transform.Canonical

static String readInput(String file) {
    return new File(file).text
}

static List<String> readInputAsLines(String file) {
    return readInput(file).trim().split('\n')
}

List<String> lines = readInputAsLines('input.txt')
//lines = readInputAsLines('other.txt')

@Canonical
class Clay {
    int fromX
    int toX
    int fromY
    int toY

    boolean contains(x, y) {
        x >= fromX && x <= toX && y >= fromY && y <= toY
    }
}

List<Clay> clays = lines.collect { line ->
    String[] part = line.split('[, =]+')
    int fromX
    int toX
    int fromY
    int toY
    int[] corrds1 = part[1].split("\\.\\.").collect { it as int }
    int[] corrds2 = part[3].split("\\.\\.").collect { it as int }
    if (part[0] == 'x') {
        fromX = corrds1[0]
        toX = corrds1.size() > 1 ? corrds1[1] : corrds1[0]
        fromY = corrds2[0]
        toY = corrds2.size() > 1 ? corrds2[1] : corrds2[0]
    } else {
        fromY = corrds1[0]
        toY = corrds1.size() > 1 ? corrds1[1] : corrds1[0]
        fromX = corrds2[0]
        toX = corrds2.size() > 1 ? corrds2[1] : corrds2[0]
    }
    new Clay(fromX, toX, fromY, toY)
}

def printClays(List<Clay> clays, Set<List<Integer>> claysSet, Set<List<Integer>> water) {
    for (int j = clays.fromY.min(); j <= clays.toY.max(); ++j) {
        for (int i = clays.fromX.min() - 1; i <= clays.toX.max() + 1; ++i) {
            if (water.contains([i, j])) {
                print('~')
            } else if (isClay(claysSet, i, j)) {
                print('#')
            } else {
                print('.')
            }
        }
        println()
    }
}

boolean isClay(claysSet, x, y) {
    claysSet.contains([x, y])
}

boolean isFilled(filled, x, y) {
    filled.contains([x, y])
}

enum ActionType {
    GoUp,
    Leak,
    Nop
}

@Canonical
class Action {
    ActionType type
    int curX
}

int maxCount = 0

falls = [] as Set

def goDown(claysSet, filled, x, y) {
//    println("Cur [$x,$y]")
    int maxY = claysSet.collect { it[1] }.max()
    while (!isClay(claysSet, x, y + 1) && y + 1 <= maxY) {
        ++y
        filled << [x, y]
    }
    if (y + 1 > maxY) {
//        println("Reaching end $x")
        return
    }
//    if (isFilled(filled, x - 1, y) || isFilled(filled, x + 1, y)) {
//        return
//    }
    //try to fill
//    println("In res [$x, $y]")
//    int i = x
//    while (!isClay(claysSet, i - 1, y)) {
//        --x
//    }
//    int j = y
//    while (isClay(claysSet, i - 1, j)) {
//        --j
//    }
    Action boundLeft = fillLeft(x, claysSet, y, filled)
    Action boundRight = fillRight(x, claysSet, y, filled)
    while (boundLeft.type == ActionType.GoUp && boundRight.type == ActionType.GoUp) {
        --y
        boundLeft = fillLeft(x, claysSet, y, filled)
        boundRight = fillRight(x, claysSet, y, filled)
    }
    if (boundLeft.type == ActionType.Leak) {
//        println("Leaking left $boundLeft.curX")
        goDown(claysSet, filled, boundLeft.curX, y)
    }
    if (boundRight.type == ActionType.Leak) {
//        println("Leaking right $boundRight.curX")
        goDown(claysSet, filled, boundRight.curX, y)
    }
}

private Action fillRight(int x, Set claysSet, int y, Set filled) {
//    println("Filling right")
    int cur = x
    while (true) {
        int nextX = cur + 1
        if (isClay(claysSet, nextX, y)) {
//            println("Next is clay")
            return new Action(ActionType.GoUp)
        }
        filled << [nextX, y]
        boolean nextIsClay = isClay(claysSet, nextX, y + 1)
        boolean nextIsFilled = filled.contains([nextX, y + 1])
        if (!nextIsClay && !nextIsFilled) {
//            println("Falling down on right")
            return new Action(ActionType.Leak, nextX)
//        } else if (nextIsClay && filled.contains([nextX + 1, y + 1])) {
////            println("Falling down on right")
//            return new Action(ActionType.Nop)
        }
        ++cur
        continue
    }
}

private Action fillLeft(int x, Set claysSet, int y, Set filled) {
    int cur = x
    while (true) {
        int prevX = cur - 1
        if (isClay(claysSet, prevX, y)) {
//            println("Prev is clay")
            return new Action(ActionType.GoUp)
        }
        filled << [prevX, y]
        boolean prevIsClay = isClay(claysSet, prevX, y + 1)
        boolean prevIsFilled = filled.contains([prevX, y + 1])
        if (!prevIsClay && !prevIsFilled) {
//            println('Falling down on left')
            return new Action(ActionType.Leak, prevX)
//        } else if (prevIsClay && filled.contains([prevX - 1, y + 1])) {
////            println("Falling down on right")
//            return new Action(ActionType.Nop)
        }
        --cur
        continue
    }
}


println("Reading clays")
Set<List<Integer>> claysSet = [] as Set
clays.each {
    for (int j = it.fromY; j <= it.toY; ++j) {
        for (int i = it.fromX; i <= it.toX; ++i) {
            claysSet << [i, j]
        }
    }
}

println("Calculating")
//for (int x = clays.fromX.min() - 1; x <= clays.toX.max() + 1; ++x) {
for (int x = 500; x <= 500; ++x) {
    println("Checking $x")
    Set<List<Integer>> filled = [] as Set
    int y = clays.fromY.min() - 1
    goDown(claysSet, filled, x, y)
    printClays(clays, claysSet, filled)
    if (filled.size() > maxCount) {
        maxCount = filled.size()
    }
    println("Current max is $maxCount")
    println('------------')
}
println(maxCount)