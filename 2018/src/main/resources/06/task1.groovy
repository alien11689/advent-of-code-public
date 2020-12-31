import groovy.transform.ToString

def text = new File('input.txt').text.trim()

def lines = text.split('\n')

@ToString
class Point {
    int x
    int y
    int id

    def isPoint(x, y) {
        this.x == x && this.y == y
    }

    def distance(int x, int y) {
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
    Set<Integer> nearestPoints
}

def board = (0..(points.max { it.y }.y + 1)).collect { y ->
    (0..(points.max { it.x }.x + 1)).collect { x ->
        Map<Integer, Integer> distances = points.collectEntries { [(it.id): it.distance(x, y)] }
        int min = distances.min { it.value }.value
        def mins = distances.findAll { it.value == min }
        Set<Integer> ids = mins.collect { it.key }
        new Cell(nearestPoints: ids)
    }
}

Map mins1 = points.collectEntries { p ->
    int count = board.flatten().findAll { Cell c -> c.nearestPoints == [p.id] as Set }.size()
//    println("${p.id} -> $count")
    [(p.id): count]
}

def board2 = (-10..(points.max { it.y }.y + 10)).collect { y ->
    (-10..(points.max { it.x }.x + 10)).collect { x ->
        Map<Integer, Integer> distances = points.collectEntries { [(it.id): it.distance(x, y)] }
        int min = distances.min { it.value }.value
        def mins = distances.findAll { it.value == min }
        Set<Integer> ids = mins.collect { it.key }
        new Cell(nearestPoints: ids)
    }
}

Map mins2 = points.collectEntries { p ->
    int count = board2.flatten().findAll { Cell c -> c.nearestPoints == [p.id] as Set }.size()
//    println("${p.id} -> $count")
    [(p.id): count]
}

int min = mins1.findAll { it.value == mins2[it.key] }.max { it.value }.value

println(min)
