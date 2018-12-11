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
class Point {
    int x
    int y
    int sum
}

List<Point> calculateRacks(board) {
    (1..(board.size() - 2)).collect { y ->
        (1..(board[0].size() - 2)).collect { x ->
            int ix = x - 1
            int iy = y - 1
            int sum = (iy..(iy + 2)).collect { i ->
                (ix..(ix + 2)).collect { j ->
                    board[i][j]
                }.sum()
            }.sum()
            new Point(x: x, y: y, sum: sum)
        }
    }.flatten()
}

def findLargestRack(List<Point> racks) {
    racks.max { it.sum }
}

//assert findLargestRack(calculateRacks(buildBoard(5, 5, 18))) == new Point(x: 2, y: 2, sum: 29)
//assert findLargestRack(calculateRacks(buildBoard(5, 5, 42))) == new Point(x: 21, y: 61, sum: 30)

def board = buildBoard(n, m, serialNumber)
def racks = calculateRacks(board)
println(findLargestRack(racks))


