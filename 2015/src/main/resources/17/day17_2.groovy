List<Integer> input = new File('day17.data').text.split('\n').collect {it as int}

int dest = 150

combinations = []

def calc(int toFill, List<Integer> containers, List<Integer> current){
    if(toFill < 0){
        return 0
    }
    if(toFill == 0){
        combinations << current
        return 1
    }
    if(containers.size() == 0){
        return 0
    }
    return calc(toFill - containers[0], containers.size() > 1 ? containers[1..-1] : [], current + containers[0]) + calc(toFill, containers.size() > 1 ? containers[1..-1] : [], current)
}

println calc(dest, input, [])

def minSize = combinations.collect {it.size()}.min()
println combinations.findAll {it.size() == minSize}.sum {1}
