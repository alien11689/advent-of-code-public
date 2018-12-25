def input = 640441
//input = 51589
//input = 59414

List<Integer> scores = [3, 7]

int steps = 9

def elves = [0, 1]

String inputString = input as String
List inputAsList = inputString as List

while (true) {
//    println(elves)
//    println(scores)
//    println('----------')
//    if (scores.size() % 1000 == 0) {
//        println(scores.size())
//    }
    int newValue = scores[elves[0]] + scores[elves[1]]
    scores.addAll(newValue.toString().collect { it as int })
    elves[0] = (elves[0] + scores[elves[0]] + 1) % scores.size()
    elves[1] = (elves[1] + scores[elves[1]] + 1) % scores.size()

    if (scores.size() > inputString.size()) {
        String substring = scores.subList(scores.size() - inputString.size() - 1, scores.size()).join()
        if(substring.contains(inputString)){
            int toAdd = substring.indexOf(inputString)
            println(scores.size() - inputString.size() - (toAdd ? 0 : 1))
            break
        }
    }
}

