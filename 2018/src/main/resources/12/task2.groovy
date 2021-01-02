import groovy.transform.ToString

String myInput = '##.#..#.#..#.####.#########.#...#.#.#......##.#.#...##.....#...#...#.##.#...##...#.####.##..#.#..#.'.trim()

def readRules(String rawRules) {
    return rawRules.trim()
            .split('\n')
            .collect {
        def split = it.split(" => ")
        new Rule(from: split[0].trim(), to: split[1].trim())
    }

}

String myRawRules = "..#.. => .\n" +
        "..#.# => .\n" +
        "#.#.. => .\n" +
        ".#..# => .\n" +
        "#.... => .\n" +
        "....# => .\n" +
        ".#.#. => #\n" +
        "#.### => .\n" +
        "####. => .\n" +
        "..... => .\n" +
        ".#... => #\n" +
        "##### => #\n" +
        ".#### => .\n" +
        "#..#. => #\n" +
        "#...# => #\n" +
        ".###. => .\n" +
        "###.# => #\n" +
        "...## => #\n" +
        "#.##. => #\n" +
        ".#.## => #\n" +
        "##.#. => #\n" +
        "...#. => .\n" +
        "..### => #\n" +
        "###.. => #\n" +
        "##... => .\n" +
        "..##. => .\n" +
        ".##.# => .\n" +
        "##.## => .\n" +
        ".##.. => .\n" +
        "##..# => #\n" +
        "#.#.# => .\n" +
        "#..## => #"

String initialState = myInput
String rawRules = myRawRules

List<Rule> rules = readRules(rawRules)

@ToString
class Rule {
    String from
    String to

    String tryApply(String slice) {
        if (slice == from) {
            to
        } else null
    }
}

int generation = 0
Map<Integer, String> state = [:].withDefault { '.' }
initialState.split('').eachWithIndex { it, idx ->
    state[idx] = it
}

def printState(Map state) {
    int min = state.findAll { it.value == '#' }.min { it.key }.key
    int max = state.findAll { it.value == '#' }.max { it.key }.key
    println("first index = $min")
    for (int i = min; i < max + 1; ++i) {
        print(state[i])
    }
    println()
}

def stateAsString(Map state) {
    int min = state.findAll { it.value == '#' }.min { it.key }.key
    int max = state.findAll { it.value == '#' }.max { it.key }.key
    StringBuilder sb = new StringBuilder()
    for (int i = min; i < max + 1; ++i) {
        sb.append(state[i])
    }
    return sb.toString()
}

long maxIter = 20
maxIter = 50000000000

while (true) {
    generation++
    Set<Integer> nonEmpty = state.findAll { it.value == '#' }.keySet()
    int minNonEmpty = nonEmpty.min()
    Set<Integer> keys = state.keySet()
    keys.findAll { it < minNonEmpty }.each { state.remove(it) }
    int maxNonEmpty = nonEmpty.max()
    keys.findAll { it > maxNonEmpty }.each { state.remove(it) }
    int max = state.keySet().max()
    int min = state.keySet().min()
    state[min - 4] = '.'
    state[min - 3] = '.'
    state[min - 2] = '.'
    state[min - 1] = '.'
    state[max + 1] = '.'
    state[max + 2] = '.'
    state[max + 3] = '.'
    state[max + 4] = '.'
    Map<Integer, String> newGeneration = state.collectEntries { it }
    int min1 = minNonEmpty - 4
    int max1 = maxNonEmpty + 4
    for (int i = min1; i <= max1; ++i) {
        StringBuilder sb = new StringBuilder()
        for (int j = i - 2; j <= i + 2; ++j) {
            sb.append(state[j])
        }
        String slice = sb.toString()
        String toReplace = rules.collect { it.tryApply(slice) }.find()
        if (toReplace) {
            newGeneration[i] = toReplace
        } else {
            newGeneration[i] = '.'
        }
    }
    if (stateAsString(state) == stateAsString(newGeneration)) {
        int base = state.findAll { it.value == '#' }.collect { it.key }.sum()
        println(base)
        int stepAfterBase = newGeneration.findAll { it.value == '#' }.collect { it.key }.sum() - base
        println(stepAfterBase)
        println((maxIter - generation + 1) * stepAfterBase + base)
        break
    }
    state = newGeneration
}
