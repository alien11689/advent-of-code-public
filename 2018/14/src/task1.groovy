def input = 640441
//input = 2018

List<Integer> scores = [3, 7]

int steps = 9

def elves = [0, 1]

int curStep = 0

while (scores.size() < input + steps + 1) {
//    println(elves)
//    println(scores)
//    println('----------')
//    if(scores.size() % 1000 == 0){
//        println(scores.size())
//    }
    int newValue = scores[elves[0]] + scores[elves[1]]
    scores.addAll(newValue.toString().collect { it as int })
    elves[0] = (elves[0] + scores[elves[0]] + 1) % scores.size()
    elves[1] = (elves[1] + scores[elves[1]] + 1) % scores.size()
}

//println(scores)
println(scores.subList(input, input + steps + 1).join())