package pl.touk.dpr.aoc2021

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
        print("Finished in ${Duration.ofMillis(end - start)}")
    }
}
