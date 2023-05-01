package dpr.eulerproject

import java.time.Duration
import java.util.function.Supplier
import kotlin.math.sqrt

object Util {
    fun getFileContent(fileName: String): String = javaClass.getResource(fileName).readText()
    fun getLinesFromFile(fileName: String): List<String> = getFileContent(fileName).lines()
    fun getNotEmptyLinesFromFile(fileName: String): List<String> = getLinesFromFile(fileName).filter { it.isNotEmpty() }

    fun <A> test(given: A, expected: A) {
        if (given != expected) {
            throw RuntimeException("$given != $expected")
        } else {
            println("Passed $given == $expected")
        }
    }

    fun measureTimeAndPrint(r: Supplier<Any?>) {
        val start = System.currentTimeMillis();
        val res = r.get()
        val end = System.currentTimeMillis();
        if (res != null) {
            println(res)
        }
        println("Finished in ${Duration.ofMillis(end - start)}")
    }

    fun measureTime(r: Runnable) {
        val start = System.currentTimeMillis();
        r.run()
        val end = System.currentTimeMillis();
        println("Finished in ${Duration.ofMillis(end - start)}")
    }

    fun isPrime(n: Long): Boolean {
        when {
            n < 2L -> return false
            n == 2L || n == 3L -> return true
            else -> {
                var i = 2L
                while (i <= sqrt(n.toDouble())) {
                    if (n % i == 0L) {
                        return false
                    }
                    ++i
                }
            }
        }
        return true
    }

    fun properDividers(value: Int): Set<Int> {
        val divs = mutableSetOf(1)
        var i = 2
        while (i <= sqrt(value.toDouble())) {
            if (value % i == 0) {
                divs.add(i)
                divs.add(value / i)
            }
            ++i
        }
        return divs
    }

}
