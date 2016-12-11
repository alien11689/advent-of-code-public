def main(List<Set<Object>> floors) {
    def memory = [] as HashSet
    Queue<Move> q = new LinkedList()
    def first = new Move(0, new Stage(0, floors))
    q << first
    memory << first.stage
    while(!q.empty){
        Move move = q.poll()
        println move
        if(isFinished(move.stage.floors)){
            return move.stepCount
        }
        int next = move.stepCount + 1
        generateMoves( move.stage.e, move.stage.floors, memory)
            .each {
               memory << it
               q.offer (new Move(next, it))
            }
    }
    return null
}

def generateMoves(int elevator, List<Set<Object>> floors, def memory){
    def currentFloor = floors[elevator]
    def possibleMoves = [currentFloor, currentFloor].combinations()
        .collect {it as Set}
        .inject([] as Set){ cur, a ->
            cur << a
        }
        .collect {
            if(isValid(currentFloor - it)){
                it
            }else {
                null
            }
        }.findAll {it}
        .collectMany { m ->
            [elevator + 1, elevator - 1].findAll {it in (0..<4)}.collect { e -> 
                List<Set<String>> newFloors = floors.collect { it.collect {it} as Set}
                newFloors[e] = newFloors[e] + m
                newFloors[elevator] = newFloors[elevator] - m
                if(isValid(newFloors[e])){
                    def st = new Stage(e, newFloors)
                    st in memory ? null : st
                }else {
                    null
                }
            
            }
        }.findAll()
}

@groovy.transform.Immutable
class Stage {
    int e
    List<Set<Object>> floors
}

@groovy.transform.Immutable
class Move {
    int stepCount
    Stage stage
}

def isValid(Set<Objects> floor){
    def gs = floor.findAll {it instanceof  G }
    gs.empty || floor.findAll {it instanceof M}.every {m ->
        gs.any {g -> g.type == m.type}
    }
}

def isFinished(List floors){
    floors[0..2].every {it.empty}
}


@groovy.transform.Immutable
class M {
    String type
}
@groovy.transform.Immutable
class G {
    String type
}

println(main(
    [
        [new M('H'), new M('L')] as Set,
        [new G('H')] as Set,
        [new G('L')] as Set,
        [] as Set
    ]
))
println()

println(main(
    [
        [new G('Po'),new  G ('T'),new M ('T'),new G ('Pr'),new G ('R'),new M ('R'),new G ('C'),new M ('C'), new G('E'), new M('E'), new G('D'), new M('D') ]  as Set,
        [new M ('Po'),new M ('Pr')] as Set,
        [] as Set,
        [] as Set,
    ]
))

