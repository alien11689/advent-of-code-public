import groovy.transform.ToString

int playersCount = 465
int maxScore = 71498

@ToString
class Player {
    int id
    long score = 0


    def addScore(int score) {
        this.score += score
    }
}

@ToString(includes = ['value'])
class Node {
    int value
    Node prev = null
    Node next = null

    Node insertAfter(int score) {
        Node newNode = new Node(value: score)
        newNode.prev = this.next
        newNode.next = this.next.next
        this.next.next.prev = newNode
        this.next.next = newNode
        return newNode
    }

    def removeBehind(int steps) {
        Node toRemove = this
        int count = 0
        while (count < 7) {
            count++
            toRemove = toRemove.prev
        }
        int value = toRemove.value
        Node newCur = toRemove.next
        toRemove.prev.next = toRemove.next
        toRemove.next.prev = toRemove.prev
        return [newCur, value]
    }
}

def printMarbles(Node root) {
    List values = [root.value]
    Node cur = root
    while (cur.next != root) {
        values << cur.next.value
        cur = cur.next
    }
    println(values)
}

def findHighestScore(int playersCount, int maxScore) {
    List<Player> players = (1..playersCount).collect { new Player(id: it) }

    Node root = new Node(value: 0)
    root.prev = root
    root.next = root

    int score = 1
    int curPlayer = 0

    Node cur = root

    while (score <= maxScore) {
//        printMarbles(root)
//        println("Cur ${cur}")
        if (score % 23 == 0) {
            players[curPlayer].addScore(score)
            List result = cur.removeBehind(8)
            players[curPlayer].addScore(result[1])
            cur = result[0]
        } else {
            cur = cur.insertAfter(score)
        }
        score++
        curPlayer = (curPlayer + 1) % playersCount
    }
//    printMarbles(root)
    return players.max { it.score }.score
}

println("PLayers count: 9, maxScore: 25, highestScore: ${findHighestScore(9, 25)}")
println("PLayers count: 10, maxScore: 1618, highestScore: ${findHighestScore(10, 1618)}")
println("PLayers count: 13, maxScore: 7999, highestScore: ${findHighestScore(13, 7999)}")
println("PLayers count: $playersCount, maxScore:$maxScore, highestScore: ${findHighestScore(playersCount, maxScore)}")
println("PLayers count: $playersCount, maxScore:${maxScore * 100}, highestScore: ${findHighestScore(playersCount, maxScore * 100)}")
