List<Integer> input = new File('day17.data').text.split('\n').collect {it as int}

int dest = 150

def calc(int toFill, List<Integer> containers){
    if(toFill < 0){
        return 0
    }
    if(toFill == 0){
        return 1
    }
    if(containers.size() == 0){
        return 0
    }
    return calc(toFill - containers[0], containers.size() > 1 ? containers[1..-1] : []) + calc(toFill, containers.size() > 1 ? containers[1..-1] : [])
}

println calc(dest, input)
