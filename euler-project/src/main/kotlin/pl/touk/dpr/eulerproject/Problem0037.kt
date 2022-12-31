package pl.touk.dpr.eulerproject

import java.lang.StringBuilder

object Problem0037 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val primes = mutableSetOf<String>()
        val repeatablePrimes = mutableSetOf<Long>()
        var i = 1L
        while(true){
            ++i
            if(!Util.isPrime(i)){
                continue
            }
            primes.add(i.toString())
            if(i >= 10){
                val asString = i.toString()
                if(
                ((0 until (asString.length)).map { asString.substring(it) } +
                        (0 until (asString.length)).map { asString.substring(0, asString.length - it) }).toSet()
                        .all { it in primes }) {
                    repeatablePrimes.add(i)
//                    println("Found $i - > $repeatablePrimes")
                }
            }
            if(repeatablePrimes.size == 11){
                println(repeatablePrimes.sum())
                break
            }
        }
    }
}
