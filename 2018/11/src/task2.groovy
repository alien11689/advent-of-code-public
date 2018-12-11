import groovy.transform.Canonical
import groovy.transform.EqualsAndHashCode
import groovy.transform.ToString

int serialNumber = 7165

int n = 300
int m = 300

int getFuelLevel(int x, int y, int serialNumber) {
    int rackId = x + 10
    long powerLevel = (rackId * y + serialNumber) * rackId
    if (powerLevel > 99) {
        ((powerLevel as String)[-3] as int) - 5
    } else {
        -5
    }
}

assert getFuelLevel(3, 5, 8) == 4
assert getFuelLevel(122, 79, 57) == -5
assert getFuelLevel(217, 196, 39) == 0
assert getFuelLevel(101, 153, 71) == 4


def buildBoard(n, m, serialNumber) {
    (1..n).collect { y ->
        (1..m).collect { x ->
            getFuelLevel(x, y, serialNumber)
        }
    }

}

@EqualsAndHashCode
@ToString
@Canonical
class Point2 {
    int x
    int y
    int sum
    int rackSize
}

Point2 calculateRacksMax(List<List<Integer>> board) {
    int maxFuel = -1000000
    Point2 currentMax = null
    (1..(board.size())).each { y ->
        (1..(board[0].size())).each { x ->
            int rackSize = 0
            Point2 current = new Point2(x, y, board[y - 1][x - 1] as int, rackSize + 1)
            rackSize++
            while (rackSize + x <= board.size() && rackSize + y <= board.size()) {
                int additionalX = x + rackSize
                int additionalY = y + rackSize
                int rightSide = (y..additionalY).collect { i ->
                    board[i - 1][additionalX - 1]
                }.sum()

                int bottomSide = (x..additionalX).collect { j ->
                    board[additionalY - 1][j - 1]
                }.sum()
                int newSize = current.sum + rightSide + bottomSide - board[additionalY - 1][additionalX - 1]
                current = new Point2(x, y, newSize, rackSize + 1)
                if (current.sum > maxFuel) {
                    maxFuel = current.sum
                    currentMax = current
                    println("Current max $currentMax")
                }
                rackSize++
            }
        }
    }
    return currentMax
}

//assert findLargestRack(calculateRacks(buildBoard(5, 5, 18))) == new Point(x: 2, y: 2, sum: 29)
//assert findLargestRack(calculateRacks(buildBoard(5, 5, 42))) == new Point(x: 21, y: 61, sum: 30)

def board = buildBoard(n, m, serialNumber)
println(calculateRacksMax(board))


