package pl.touk.dpr.eulerproject

import java.math.BigInteger

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
//        println("$n:\t\t$cur")
        while(prev > step){
            cur = (cur + base) % mod
            if (cur < prev) {
                sum += cur
                println("Found $cur - cur sum is $sum")
                prev = cur
            }
        }
        var prevManualIncrease = 0L
        var internalBump = 0L
        while(prev > 1L){
//        while(prev > 1L && n <= 20000){
            cur += 2 * base
            val nInc = (mod - cur) / step + 1
            cur = (cur + nInc * step) % mod
//            println("Increased from $prev to $cur by ${prev - cur}")
            cur = (cur + BigInteger.valueOf(2 * 2044785486369).times(BigInteger.valueOf(prevManualIncrease)).mod(BigInteger.valueOf(step)).toLong())
            var manualIncreased = 2 * prevManualIncrease
            while (prev < cur){
                cur = (cur + 2044785486369) % step
                ++manualIncreased
            }
            println("Performed $manualIncreased increased and its ${manualIncreased - prevManualIncrease} more than previously (${manualIncreased * 1.0 / prevManualIncrease})")
            prevManualIncrease = manualIncreased
//            cur = (cur + base) % mod
//            ++n
//            println("$n:\t\t$cur")
            if (cur < prev){
                sum += cur
                val diff = prev - cur
                println("Found $cur - cur sum is $sum, diff $diff")
                internalBump = 0L
                while (cur > diff){
                    cur -= diff
                    sum += cur
                    println("Found $cur  - cur sum is $sum")
                    ++internalBump
                }
                println("Performed $internalBump internal bumps")
                if (cur == 17L && diff == 30L){
                    // Based on observation
                    // > Found 77 - cur sum is 1517926517777482, diff 30
                    // > Found 47  - cur sum is 1517926517777529
                    // > Found 17  - cur sum is 1517926517777546
                    // > Performed 2 internal bumps
                    // next found will be 30 - 17 = 13 lower than current
                    // so I expect next line to be
                    // > Found 4  - cur sum is 1517926517777550, diff X
                    // possible diff are 3, 2 or 1
                    // if it's 3 than
                    // > Found 4  - cur sum is 1517926517777550, diff 3
                    // > Found 1  - cur sum is 1517926517777551 and it's wrong
                    // if it's 2 then
                    // > Found 4  - cur sum is 1517926517777550, diff 2
                    // > Found 2  - cur sum is 1517926517777552
                    // > Found 0  - cur sum is 1517926517777552 and it's wrong
                    // if it's 1 than
                    // > Found 4  - cur sum is 1517926517777550, diff 1
                    // > Found 3  - cur sum is 1517926517777553
                    // > Found 2  - cur sum is 1517926517777555
                    // > Found 1  - cur sum is 1517926517777556
                    sum += 4 + 3 + 2 + 1
                    break
                }
                prev = cur
            }
        }
        println(sum)
    }
}
