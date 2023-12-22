package dpr.commons

enum class Dir(val dx: Int, val dy: Int) {
    N(0, -1),
    W(-1, 0),
    S(0, 1),
    E(1, 0);

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

    fun opposite(): Dir = when (this) {
        S -> N
        N -> S
        W -> E
        E -> W
    }
}
