def main(String input) {
    def maze = input.split('\n')
        .collect { line -> 
            line.collect {it}
        }
    def(initX,initY) = find0(maze)
    int expected = findNumbers(maze)

    def memory = [] as Set
    def q = new LinkedList()

    Step initStep = new Step(
        count: 0,
        stage: new Stage(
            visited: ['0'] as Set,
            x: initX,
            y: initY
        )
    )
    q << initStep
    memory << initStep.stage

    while(!q.empty){
        Step step = q.poll()
        println "q: ${q.size()}, count: ${step.count}"
        if(step.stage.visited.size() == expected){
            return step.count
        }
        def neighbours = findNeighbours(step.stage, maze)
            .findAll {!(it in memory)}
        neighbours.each {
            memory << it
            q << new Step(count: step.count + 1, stage: it)
        }
    }
}

def findNeighbours(Stage stage, def maze){
    [
        [stage.x + 1, stage.y],
        [stage.x - 1, stage.y],
        [stage.x, stage.y + 1],
        [stage.x, stage.y - 1],
    ].findAll {it[0] >= 0 && it[0] < maze[0].size() && it[1] >= 0 && it[1] < maze.size() && maze[it[1]][it[0]] != '#'}
    .collect {
        String cur = maze[it[1]][it[0]]
        new Stage(
            x: it[0],
            y: it[1],
            visited: cur != '.' ? stage.visited + cur : stage.visited
        )
    }
}

@groovy.transform.EqualsAndHashCode
class Stage {
    Set visited
    int x
    int y
}

class Step {
    int count
    Stage stage
}

def find0(def maze){
    int x
    int y
    maze.eachWithIndex { row, yi ->
        row.eachWithIndex {cell, xi ->
            if(cell == '0'){
                x = xi
                y = yi
            }   
        }
    }
    [x, y]
}
def findNumbers(def maze){
    int num = 0
    maze.eachWithIndex { row, yi ->
        row.eachWithIndex {cell, xi ->
            if(cell != '#' && cell != '.'){
                ++num
            }   
        }
    }
    num
}

println(main(new File('sample.txt').text))
println()
println(main(new File('input.txt').text))
