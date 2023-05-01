package dpr.aoc2019

import java.time.Duration
import java.util.function.Supplier

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
}
