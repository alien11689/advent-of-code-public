import groovy.transform.Canonical

def text = new File('input.txt').text
//text = new File('other.txt').text
//text = new File('other2.txt').text

def lines = text.split('\n')

enum Type {
    SLASH,
    BACKSHLASH,
    MINUS,
    PIPE,
    CROSS
}

@Canonical
class Cell {
    Type type


    @Override
    String toString() {
        switch (type) {
            case Type.SLASH: return '/'
            case Type.BACKSHLASH: return '\\'
            case Type.MINUS: return '-'
            case Type.PIPE: return '|'
            case Type.CROSS: return '+'
        }
    }
}

enum Direction {
    UP{
        Direction turnLeft() {
            LEFT
        }

        Direction turnRight() {
            RIGHT
        }
    },
    DOWN{
        Direction turnLeft() {
            RIGHT
        }

        Direction turnRight() {
            LEFT
        }
    },
    LEFT{
        Direction turnLeft() {
            Direction.DOWN
        }

        Direction turnRight() {
            Direction.UP
        }
    },
    RIGHT{
        Direction turnLeft() {
            Direction.UP
        }

        Direction turnRight() {
            Direction.DOWN
        }
    };

    abstract Direction turnLeft()

    abstract Direction turnRight()
}

enum DirOnCross {
    LEFT,
    STRAIGHT,
    RIGHT
}

@Canonical
class Driver {
    int x
    int y
    Direction direction
    DirOnCross dirOnCross = DirOnCross.LEFT

    def move(Cell[][] cells) {
        switch (direction) {
            case Direction.UP: --y; break
            case Direction.RIGHT: ++x; break
            case Direction.DOWN: ++y; break
            case Direction.LEFT: --x; break
        }

//        if (cells[y][x] == null) {
//            println("Left is ${cells[y][x - 1]}")
//            println("Right is ${cells[y][x + 1]}")
//            println("Down is ${cells[y + 1][x]}")
//            println("Driver is on $x, $y with direction $direction")
//            throw new RuntimeException()
//        }

        if (cells[y][x].type == Type.SLASH) {
            switch (direction) {
                case Direction.UP:
                    direction = Direction.RIGHT
                    break
                case Direction.RIGHT:
                    direction = Direction.UP
                    break
                case Direction.DOWN:
                    direction = Direction.LEFT
                    break
                case Direction.LEFT:
                    direction = Direction.DOWN
                    break
            }
        } else if (cells[y][x].type == Type.BACKSHLASH) {
            switch (direction) {
                case Direction.UP:
                    direction = Direction.LEFT
                    break
                case Direction.RIGHT:
                    direction = Direction.DOWN
                    break
                case Direction.DOWN:
                    direction = Direction.RIGHT
                    break
                case Direction.LEFT:
                    direction = Direction.UP
                    break
            }
        } else if (cells[y][x].type == Type.CROSS) {
            switch (dirOnCross) {
                case DirOnCross.LEFT:
                    direction = direction.turnLeft()
                    dirOnCross = DirOnCross.STRAIGHT
                    break
                case DirOnCross.STRAIGHT:
                    dirOnCross = DirOnCross.RIGHT
                    break
                case DirOnCross.RIGHT:
                    direction = direction.turnRight()
                    dirOnCross = DirOnCross.LEFT
                    break
            }
        }
    }

    @Override
    String toString() {
        switch (direction) {
            case Direction.LEFT: return '<'
            case Direction.UP: return '^'
            case Direction.RIGHT: return '>'
            case Direction.DOWN: return 'v'
        }
    }
}

Cell[][] cells = new Cell[lines.size()][lines.max { it.size() }.size()]
List<Driver> drivers = []

lines.eachWithIndex { line, y ->
    line.eachWithIndex { val, x ->
//        println("$x, $y")
        switch (val) {
            case '/': cells[y][x] = new Cell(Type.SLASH); break
            case '\\': cells[y][x] = new Cell(Type.BACKSHLASH); break
            case '-': cells[y][x] = new Cell(Type.MINUS); break
            case '|': cells[y][x] = new Cell(Type.PIPE); break
            case '+': cells[y][x] = new Cell(Type.CROSS); break
            case '^':
                Driver driver = new Driver(x, y, Direction.UP)
                drivers << driver
                cells[y][x] = new Cell(Type.PIPE)
                break
            case '>':
                Driver driver = new Driver(x, y, Direction.RIGHT)
                drivers << driver
                cells[y][x] = new Cell(Type.MINUS)
                break
            case 'v':
                Driver driver = new Driver(x, y, Direction.DOWN)
                drivers << driver
                cells[y][x] = new Cell(Type.PIPE)
                break
            case '<':
                Driver driver = new Driver(x, y, Direction.LEFT)
                drivers << driver
                cells[y][x] = new Cell(Type.MINUS)
                break
            default: break
        }
    }
}

def printBoard(Cell[][] cells, List<Driver> drivers) {
    for (int y = 0; y < cells.length; ++y) {
        for (int x = 0; x < cells[0].length; ++x) {
            if (cells[y][x] == null) {
                print(' ')
            } else {
                List<Driver> driversOnCell = drivers.findAll() { it.x == x && it.y == y }
                if (driversOnCell) {
                    if (driversOnCell.size() > 1)
                        print('X')
                    else print(driversOnCell[0])
                } else {
                    print(cells[y][x])
                }
            }
        }
        println()
    }
}

int tick = 0
//printBoard(cells, drivers)
//return

while (true) {
//    println(tick)
//    if(tick == 201 || tick == 202 | tick == 203) {
//    printBoard(cells, drivers)
//    }
    ++tick
    def crashes = []
    drivers.sort { [it.y, it.x] }.each {
        if (it in crashes) {

        } else {
            it.move(cells)
            def conflict = drivers.find { o -> it != o && o.x == it.x && o.y == it.y }
            if(conflict) {
                crashes << it
                crashes << conflict
            }
        }
    }
    crashes.each { car ->
//        println("Crash on ${car.x} ${car.y}")
        drivers.remove(car)
    }
    if (drivers.size() == 1) {
        println(drivers[0].x)
        println(drivers[0].y)
        throw new RuntimeException()
    }
}

