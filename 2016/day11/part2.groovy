def main(List<Set<Integer>> floors) {
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

def generateMoves(int elevator, List<Set<Integer>> floors, def memory){
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
        .collect { m ->
            def newE
            if(m.size() == 2){
                newE = elevator + 1
            }else {
                newE = elevator - 1
            }
            if(newE in (0..<4) && (newE != 0 || !floors[0].empty) && (!(newE == 1 && floors[0].empty &&floors[1].empty)) ){
                List<Set<String>> newFloors = floors.collect { it.collect {it} as Set}
                newFloors[newE] = newFloors[newE] + m
                newFloors[elevator] = newFloors[elevator] - m
                if(isValid(newFloors[newE])){
                    def st = new Stage(newE, newFloors)
                    st in memory ? null : st
                }else {
                    null
                }
            }else {
                null
            }
        }.findAll()
}

@groovy.transform.Immutable
class Stage {
    int e
    List<Set<Integer>> floors
}

@groovy.transform.Immutable
class Move {
    int stepCount
    Stage stage
}

@groovy.transform.Memoized
def isValid(Set<Objects> floor){
    def gs = floor.findAll {it > 0 }
    gs.empty || floor.findAll {it < 0 }.every {m ->
        gs.any {g -> m == -g}
    }
}

def isFinished(List floors){
    floors[0..2].every {it.empty}
}

println(main(
    [
        [1,2,-2,3,4,-4,5,-5,6,-6,7,-7]  as Set,
        [-1,-3] as Set,
        [] as Set,
        [] as Set,
    ]
))

