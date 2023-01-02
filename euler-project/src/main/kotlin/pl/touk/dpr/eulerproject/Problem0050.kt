package pl.touk.dpr.eulerproject

object Problem0050 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val primes = mutableListOf(2L,3L)
        var i = 3
        while (i < 1_000_000){
            i+= 2
            if(Util.isPrime(i.toLong())){
                primes.add(i.toLong())
            }
        }

        var effectivePrimesSize = primes.size
        i = effectivePrimesSize
        var maxLength = 0
        var bestPrime = 0L
        while (i >= maxLength){
            i--
            effectivePrimesSize = i - 1
            val p = primes[i]
//                println("Checking $p on pos $i")
                var cur = primes.take(2)
                var pIdx = 1
                while (pIdx < effectivePrimesSize){
                    val s = cur.sum()
                    when {
                        s == p -> {
                            val size = cur.size
                            if(size > maxLength){
                                maxLength = size
                                bestPrime = p
//                                println("Found candidate $bestPrime with $maxLength")
                            }
                            break
                        }

                        s < p -> if(pIdx + 1 == effectivePrimesSize) break
                        else {
                            if (cur[0] * 2 > i) {
                                break
                            }
                            cur = cur + primes[++pIdx]
                        }
                        s > p -> cur = cur.drop(1)
                    }
                }
        }
//        println(maxLength)
        println(bestPrime)
    }
}
