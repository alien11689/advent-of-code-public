package dpr.eulerproject

import java.lang.StringBuilder

object Problem0038 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val mem = mutableSetOf<String>()
        val pandigital = ('1'..'9').toSet()

        var num = 1
        while(num < 1_000_000_000 / 3){
            var i = 1
            val sb = StringBuilder()
            while (i < 10){
                sb.append(num * i)
                if (i > 1 && sb.length == 9){
                    val string = sb.toString()
                    if(string.toSet() == pandigital){
//                        println("Found $num with sums 1..$i gives $string")
                        mem.add(string)
                    }
                } else if(sb.length >= 9){
                    break
                }
                ++i
            }
            ++num
        }
        println(mem.max())
    }
}
