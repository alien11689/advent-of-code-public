def main(List<Set<Object>> floors) {
    def memory = [] as Set
    Queue<Move> q = new LinkedList()
    q << new Move(0, new Stage(0, floors))
    while(!q.empty){
        Move move = q.remove()
        println move
        if(isFinished(move.stage.floors)){
            return move.stepCount
        }
        generateMoves( move.stage.e, move.stage.floors)
            .findAll { !(it in memory)}
            .each {
               memory << it
               q.offer (new Move(move.stepCount + 1, it))
            }
    }
    return null
}

def generateMoves(int elevator, List<Set<Object>> floors){
    def currentFloor = floors[elevator]
    def possibleMoves = [currentFloor, currentFloor].combinations()
        .collect {it as Set}
        .inject([] as Set){ cur, a ->
            cur << a
        }
        .collect {
            def newFloor = currentFloor - it
            if(isValid(newFloor)){
                it
            }else {
                null
            }
        }.findAll {it}
    [elevator + 1, elevator -1].findAll {it in (0..<4)}.collectMany { e->
        possibleMoves.collect { pm ->
            List<Set<String>> newFloors = floors.collect { it.collect {it} as Set}
            newFloors[e] = newFloors[e] + pm
            newFloors[elevator] = newFloors[elevator] - pm
            if(isValid(newFloors[e])){
                new Stage(e, newFloors)
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
    floor.empty || floor.findAll {it instanceof M}.every {m ->
        floor.findAll {it instanceof G }.empty || floor.findAll {it instanceof G}.any {g -> g.type == m.type}
    }
}

def isFinished(List floors){
    floors[0..2].every {it.empty}
}


def canCharge(Set<Object> floor){
    floor.findAll {it instanceof M}.any {m ->
        floor.findAll {it instanceof G}.any {g -> g.type == m.type}
    }
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
        [new G('Po'),new  G ('T'),new M ('T'),new G ('Pr'),new G ('R'),new M ('R'),new G ('C'),new M ('C') ]  as Set,
        [new M ('Po'),new M ('Pr')] as Set,
        [] as Set,
        [] as Set,
    ]
))

