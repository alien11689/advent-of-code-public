import groovy.transform.ToString

//def text = new File('other.txt').text.trim()
def text = new File('input.txt').text.trim()

def lines = text.split('\n')

@ToString
class Point {
    int x
    int y
    int vx
    int vy

    def move() {
        x += vx
        y += vy
    }

    def isPoint(x, y) {
        return this.x == x && this.y == y
    }
}

List<Point> points = lines.collect {
    def line = it.split(/[<>, ]/).findAll()
    Point p = new Point(
            x: line[1] as int,
            y: line[2] as int,
            vx: line[4] as int,
            vy: line[5] as int,
    )
    return p
}

def printPoints(List<Point> points) {
    int minX = points.x.min()
    int maxX = points.x.max()
    int minY = points.y.min()
    int maxY = points.y.max()
    def ps = points.collect { p -> [p.x, p.y] }
    StringBuilder sb = new StringBuilder()
    for (int y = minY; y <= maxY; ++y) {
        for (int x = minX; x <= maxX; ++x) {
            if (ps.contains([x, y])) {
//                    sb.append("#")
                print('#')
            } else {
//                    sb.append(".")
                print('.')
            }
        }
//            sb.append('\n')
        println()
    }
    println(sb)
}

int tick = 0
while (true) {
    println("Tick $tick")
    int minX = points.x.min()
    int maxX = points.x.max()
    if (maxX - minX < 100) {
        printPoints(points)
        Thread.sleep(1000)
    }
    ++tick
    points.each { it.move() }
}