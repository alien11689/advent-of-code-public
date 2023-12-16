package dpr.aoc2023

enum class Dir {
    N,
    W,
    S,
    E;

    fun turnLeft() = when (this) {
        N -> W
        W -> S
        S -> E
        E -> N
    }

    fun turnRight() = when (this) {
        N -> E
        W -> N
        S -> W
        E -> S
    }
}
