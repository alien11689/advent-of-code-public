package pl.touk.dpr.eulerproject

object Problem0700 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val base = 1504170715041707L
        val mod = 4503599627370517L
        val step = 3 * base - mod
        println("3 * base - mod = $step")
        println("mod contains surplus = ${mod * 1.0 / step}")
        var prev = base
        var sum = base
        var cur = base
        var n = 1L
        var prevN = 1L
//        println("$n:\t\t$cur")
        while(prev > step){
            cur = (cur + base) % mod
            ++n
            if (cur < prev) {
                sum += cur
                println("Found $cur on $n - cur sum is $sum")
                prev = cur
                prevN = n
            }
        }
        while(prev > 1L){
//        while(prev > 1L && n <= 20000){
            cur += 2 * base
            n += 2
            val nInc = (mod - cur) / step + 1
            cur = (cur + nInc * step) % mod
//            println("Increased from $prev to $cur by ${prev - cur}")
            while (prev < cur){
                cur = (cur + 2044785486369) % step
            }
            n += nInc
//            cur = (cur + base) % mod
//            ++n
//            println("$n:\t\t$cur")
            if (cur < prev){
                sum += cur
                val diff = prev - cur
                val diffN = n - prevN
                println("Found $cur on $n (diffN is $diffN) - cur sum is $sum, diff $diff")
                while (cur > diff){
                    cur -= diff
                    sum += cur
                    n += diffN
                    println("Found $cur on $n (diffN is $diffN) - cur sum is $sum")
                }
                prev = cur
                prevN = n
            }
        }
        println(sum)
        // 1517926517477964 wrong
    }
}
