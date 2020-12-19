//String input = 'inputDemo1.txt'
//String input = 'inputDemo2.txt'
//String input = 'inputDemo3.txt'
String input = 'input.txt'

List<List<String>> paths = new File(input).text.split('\n').collect { it.split ','}
println paths

Set getVisited(List<String> path){
    Set mem = [] as Set
    mem << [0,0]
    def cur = [0,0]
    path.each{
        String dir = it[0]
        int dist = it.substring(1) as int 
        switch(dir){
            case 'U':
                (cur[1]..(cur[1] + dist)).each{
                    mem << [cur[0], it]
                }
                cur = [cur[0], cur[1] + dist]
                break
            case 'D':
                (cur[1]..(cur[1] - dist)).each{
                    mem << [cur[0], it]
                }
                cur = [cur[0], cur[1] - dist]
                break
            case 'L':
                (cur[0]..(cur[0] - dist)).each{
                    mem << [it, cur[1]]
                }
                cur = [cur[0] - dist, cur[1]]
                break
            case 'R':
                (cur[0]..(cur[0] + dist)).each{
                    mem << [it, cur[1]]
                }
                cur = [cur[0] + dist, cur[1]]
                break
        }
    }
    return mem
}

def a = getVisited(paths[0] as List<String>)
def b = getVisited(paths[1] as List<String>)

def solution = (a.intersect(b).collect {it[0].abs() + it[1].abs()} - [0,0]).min()
println solution
