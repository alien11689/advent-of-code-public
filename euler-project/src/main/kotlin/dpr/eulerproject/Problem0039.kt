package dpr.eulerproject

object Problem0039 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val m = mutableMapOf<Int, Int>()
        for (p in 3..1000) {
            for (a in 1..(p - 2)) {
                for (b in 1..a) {
                    val c = p - a - b
                    if (c > 0 && b * b + c * c == a * a) {
//                        println("Found $a $b $c")
                        m[p] = (m[p] ?: 0) + 1
                    }
                }
            }
        }
        println(m.maxBy { it.value }.key)
    }
}
