import groovy.transform.ToString

int playersCount = 465
int maxScore = 71498

@ToString
class Player {
    int id
    int score = 0


    def addScore(int score) {
        this.score += score
    }
}

def findHighestScore(int playersCount, int maxScore) {
    List<Player> players = (1..playersCount).collect { new Player(id: it) }

    List<Integer> marbles = [0]

    int score = 1
    int index = 0
    int curPlayer = 0

    while (score <= maxScore) {
//    println(marbles)
        if (score % 23 == 0) {
            players[curPlayer].addScore(score)
            int toRemoveIndex = (marbles.size() + index - 8) % marbles.size()
            players[curPlayer].addScore(marbles[toRemoveIndex])
            marbles.remove(toRemoveIndex)
            index = (toRemoveIndex + 1) % marbles.size()
        } else {
            marbles.add(index + 1, score)
            index = (index + 2) % marbles.size()
        }
        score++
        curPlayer = (curPlayer + 1) % playersCount
    }
//println(marbles)
    return players.max { it.score }.score
}

println("PLayers count: 9, maxScore: 25, highestScore: ${findHighestScore(9, 25)}")
println("PLayers count: 10, maxScore: 1618, highestScore: ${findHighestScore(10, 1618)}")
println("PLayers count: 13, maxScore: 7999, highestScore: ${findHighestScore(13, 7999)}")
println("PLayers count: $playersCount, maxScore:$maxScore, highestScore: ${findHighestScore(playersCount, maxScore)}")
println("PLayers count: $playersCount, maxScore:${maxScore * 100}, highestScore: ${findHighestScore(playersCount, maxScore * 100)}")