import groovy.transform.Immutable

List<String> lines = new File('input.txt').text.trim().split('\n')

@Immutable
class Point {
    int x
    int y
}

Set<Point> points = [] as Set

for (int i = 0; i < lines.size(); ++i) {
    for (int j = 0; j < lines[i].size(); ++j) {
        if (lines[i][j] == '#') {
            points << new Point(j, i)
        }
    }
}

def calcA(Point p1, Point p2) {
    if (p1.x == p2.x) {
        return null
    } else {
        return (p2.y - p1.y) / (p2.x - p1.x)
    }
}

int signum(int a, int b) {
    if (a == b) {
        return 0
    }
    if (a < b) {
        return -1
    }
    return 1
}

int solution = points.collect { Point center ->
    (points.findAll { it != center }.collect { Point other ->
        [calcA(center, other), signum(center.x, other.x), signum(center.y, other.y)]
    } as Set).size()
}.max()

println solution