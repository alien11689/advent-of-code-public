List<Step> steps = new File('input.txt').text.split(', ').collect {
    println it
    new Step(it[0] as Turn, it[1..-1] as int )
}

def nextPos = steps.inject(new Pos()){ cur, step ->
    cur.apply(step)
}

println (nextPos.x.abs() + nextPos.y.abs())


enum Turn {
    L,R
}

@groovy.transform.Immutable
class Step {
    Turn t
    int steps
}

enum Side {
    N {
        def left() { W }
        def right() { E }
        def move(int steps){[0, steps]}
    }, 
    E {
        def left() { N }
        def right() { S }
        def move(int steps){[ steps,0]}
    }, 
    S {
        def left() { E }
        def right() { W }
        def move(int steps){[0, -steps]}
    }, 
    W {
        def left() { S }
        def right() { N }
        def move(int steps){[-steps,0]}
    }, 
}

@groovy.transform.Immutable
class Pos {
    Side s = Side.N
    int x = 0
    int y = 0

    Pos apply(Step step){
        Side newSide = step.t == Turn.L ? s.left() : s.right()
        def (dx, dy) = newSide.move(step.steps)
        new Pos(newSide, x + dx, y + dy)
    }
}
