String input = 'inputDemo1.txt'
input = 'inputDemo2.txt'
input = 'inputDemo3.txt'
input = 'input.txt'

List<List<String>> paths = new File(input).text.split('\n').collect { it.split ','}
println paths

Set getVisited(List<String> path){
    Set mem = [] as Set
    def cur = [0,0]
    int step = 0
    path.each{
        String dir = it[0]
        int dist = it.substring(1) as int 
        switch(dir){
            case 'U':
                ((cur[1]+1)..(cur[1] + dist)).each{
                    mem << [cur[0], it, ++step]
                }
                cur = [cur[0], cur[1] + dist]
                break
            case 'D':
                ((cur[1]-1)..(cur[1] - dist)).each{
                    mem << [cur[0], it, ++step]
                }
                cur = [cur[0], cur[1] - dist]
                break
            case 'L':
                ((cur[0]-1)..(cur[0] - dist)).each{
                    mem << [it, cur[1], ++step]
                }
                cur = [cur[0] - dist, cur[1]]
                break
            case 'R':
                ((cur[0]+1)..(cur[0] + dist)).each{
                    mem << [it, cur[1], ++step]
                }
                cur = [cur[0] + dist, cur[1]]
                break
        }
    }
    return mem
}

def a = getVisited(paths[0] as List<String>)
def b = getVisited(paths[1] as List<String>)

def intersections = ((a.collect {[it[0], it[1]]}.intersect(b.collect {[it[0],it[1]]})) as Set) //- [0,0]
intersections.remove([0,0])
println intersections

def res = intersections.collect {inter -> 
    a.findAll {cur -> [cur[0], cur[1]] == inter}
        .collect {it[2]}.min() + 
    b.findAll {cur -> [cur[0], cur[1]] == inter}
        .collect {it[2]}.min()
    }.min()

println res
