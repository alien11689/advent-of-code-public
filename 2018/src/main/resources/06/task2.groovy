import groovy.transform.ToString

def text = new File('input.txt').text.trim()
int limit = 10000

def lines = text.split('\n')

@ToString
class Point {
    int x
    int y
    int id

    def isPoint(x, y) {
        this.x == x && this.y == y
    }

    int distance(int x, int y) {
        Math.abs(this.x - x) + Math.abs(this.y - y)
    }
}

int counter = ('a' as char) as int
def points = lines.collect {
    def parts = it.split(',')
    new Point(x: parts[0] as int, y: parts[1] as int, id: ++counter)
}

@ToString
class Cell {
    int x
    int y
    int distance

    def isNeighbour(Cell o) {
        Math.abs(o.x - this.x) + Math.abs(o.y - this.y) <= 1
    }

    def isPoint(x, y) {
        this.x == x && this.y == y
    }
}


def board = (-5..(points.max { it.y }.y + 5)).collect { y ->
    (-5..(points.max { it.x }.x + 5)).collect { x ->
        int dist = points.collect { it.distance(x, y) }.sum()
        def cell = new Cell(x: x, y: y, distance: dist)
        cell
    }
}

List inBound = board.flatten().findAll { it.distance < limit }
println(inBound.size())
