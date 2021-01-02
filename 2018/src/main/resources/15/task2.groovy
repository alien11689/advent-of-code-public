import groovy.transform.Canonical

enum PlayerType {
    E,
    G
}

@Canonical
class Position implements Comparable<Position> {
    int x
    int y

    List<Position> neighbours() {
        [
                new Position(x + 1, y),
                new Position(x - 1, y),
                new Position(x, y - 1),
                new Position(x, y + 1),
        ]
    }

    int compareTo(Position o) {
        int dy = y - o.y
        if (dy != 0) {
            dy
        } else {
            x - o.x
        }
    }
}

@Canonical
class Player implements Comparable<Player> {
    int x
    int y
    PlayerType type
    int hitPoints
    int attackPower
    boolean moved = false

    boolean isDead() {
        hitPoints <= 0
    }

    int compareTo(Player o) {
        int dy = y - o.y
        if (dy != 0) {
            dy
        } else {
            x - o.x
        }
    }

    List<Position> neighbours() {
        [
                new Position(x + 1, y),
                new Position(x - 1, y),
                new Position(x, y - 1),
                new Position(x, y + 1),
        ]
    }

    List<Position> inRange(List<Player> players, CellType[][] board) {
        neighbours().findAll { Position p ->
            board[y][x] == CellType.FREE && !players.any {
                !it.dead && p.x == it.x && p.y == it.y
            }
        }
    }

    def move(List<Player> players, CellType[][] board) {
        findNextMove(players, board).each { it.action() }
    }

    List<Action> findNextMove(List<Player> players, CellType[][] board) {
        List<Player> enemies = neighbours().collect { Position n ->
            players.find { !it.dead && it.type != type && it.onPosition(n) }
        }.findAll()
        if (enemies) {
//                    println("Attack $enemy")
            return [new Attack(enemies.sort().sort { it.hitPoints }.first(), this)]
        }
//        println("I won't attack an enemy")
        Set<Position> memory = [] as Set
        memory << new Position(x, y)
        Queue<PositionWithDist> queue = new PriorityQueue<PositionWithDist>({ o1, o2 ->
            if (o1.dist == o2.dist) {
                o1.pos <=> o2.pos
            } else
                return o1.dist <=> o2.dist
        } as Comparator)
        queue << new PositionWithDist(new Position(x, y), 0, [])
        while (!queue.empty) {
            try {
                PositionWithDist cur = queue.poll()
                List<Position> neighbours = cur.pos.neighbours()
                neighbours.each { Position n ->
//                    println("Checking position $n")
                    if (!(n in memory)) {
                        if (board[n.y][n.x] == CellType.CLOSED) {
                            memory << n
                        } else {
                            Player maybePlayer = players.find { !it.dead && it.onPosition(n) }
                            if (maybePlayer) {
//                                println("Neighbour $maybePlayer")
                                if (maybePlayer.type != type) {
//                                    println("Moving... to ${cur.road[0]}")
                                    throw new MoveOccured(new Move(this, cur.road[0]))
                                }
                            } else {
                                PositionWithDist next = new PositionWithDist(n, cur.dist + 1, cur.road.collect())
                                next.road << n
                                queue << next
                                memory << n
                            }
                        }
                    }
                }
            } catch (MoveOccured e) {
                List<Player> es = e.move.newPosition.neighbours().collect { Position n ->
                    players.find { !it.dead && it.type != type && it.onPosition(n) }
                }.findAll()
                if (es) {
//                    println("Move and attack $enemy")
                    return [e.move, new Attack(es.sort().sort { it.hitPoints }.first(), this)]
                }
                return [e.move]
            }
        }
        return [new Nop()]
    }

    boolean onPosition(Position p) {
        x == p.x && y == p.y
    }
}

@Canonical
class MoveOccured extends RuntimeException {
    Move move
}

interface Action {
    void action()
}

@Canonical
class Move implements Action {
    Player p
    Position newPosition

    void action() {
        p.x = newPosition.x
        p.y = newPosition.y
    }
}

@Canonical
class Attack implements Action {
    Player enemy
    Player attacker

    void action() {
        enemy.hitPoints -= attacker.attackPower
    }
}

@Canonical
class Nop implements Action {

    @Override
    void action() {

    }
}

@Canonical
class PositionWithDist {
    Position pos
    int dist
    List<Position> road = []
}

enum CellType {
    CLOSED,
    FREE
}

def buildBoard(List<String> lines) {
    return lines.collect { line ->
        line.collect { it == '#' ? CellType.CLOSED : CellType.FREE } as CellType[]
    } as CellType[][]
}


def buildPlayers(List<String> lines, int attack) {
    List<Player> players = []
    for (int y = 0; y < lines.size(); ++y) {
        for (int x = 0; x < lines[y].size(); ++x) {
            if (lines[y][x] == 'G') {
                players << new Player(x, y, PlayerType.G, 200, 3)
            } else if (lines[y][x] == 'E') {
                players << new Player(x, y, PlayerType.E, 200, attack)
            }
        }
    }
    return players
}

int game(List<Player> players, CellType[][] board) {
    int round = 0
//    println(round)
//    printBoard(players, board)
    try {
        while (true) {
            players.each {
                it.moved = false
            }
            List<Player> orderedPlayers = players.collect().sort()
            orderedPlayers.each { player ->
//                println("Player $player")
                if (!player.dead) {
                    player.move(players, board)
                    player.moved = true
                    if (!players.any { !it.dead && it.type != player.type }) {
                        players.removeAll { it.dead }
                        if (players.every { it.moved }) {
                            ++round
                        }
                        throw new End(round)
                    }
                }
            }
            if (players.find { it.type == PlayerType.E && it.dead }) {
                throw new ElfDied()
            }
            players.removeAll { it.dead }
            ++round
        }
    } catch (End end) {

        return end.round
    }
}

@Canonical
class ElfDied extends RuntimeException {
}

@Canonical
class End extends RuntimeException {
    int round
}

void printBoard(List<Player> players, CellType[][] board) {
    for (int y = 0; y < board.length; ++y) {
        for (int x = 0; x < board[y].length; ++x) {
            Player p = players.find { it.onPosition(new Position(x, y)) }
            if (p) {
                print(p.type)
            } else {
                print(board[y][x] == CellType.FREE ? '.' : '#')
            }
        }
        println()
    }
    players.sort().each { println it }
}

def whole(String file) {
    String text = new File(file).text.trim()
    List<String> lines = text.split('\n')
    CellType[][] board = buildBoard(lines)
    int minBound = 4
    int maxBound = 100
    int maxSum = -1
    while(minBound <= maxBound){
	int i = (minBound + maxBound) / 2
        List<Player> players = buildPlayers(lines, i)
        int round
        try {
            round = game(players, board)
            int sum = players.sum { it.hitPoints }

            maxSum = (round * sum)
	    maxBound = i - 1
        } catch (ElfDied e){
	    minBound = i + 1
        }
    }
    return maxSum
}

println(whole('input.txt'))
