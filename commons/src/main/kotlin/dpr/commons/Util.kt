package dpr.commons

import java.time.Duration
import java.util.function.Supplier

object Util {
    fun getFileContent(fileName: String): String = javaClass.getResource(fileName).readText()
    fun getLinesFromFile(fileName: String): List<String> = getFileContent(fileName).lines()
    fun getNotEmptyLinesFromFile(fileName: String): List<String> = getLinesFromFile(fileName).filter { it.isNotEmpty() }

    fun <T> readBoard(lines: List<String>, valueMapper: (Char) -> T): MutableMap<Point2D, T> {
        val board = mutableMapOf<Point2D, T>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                board[Point2D(x, y)] = valueMapper(c)
            }
        }
        return board
    }

    fun readBoard(lines: List<String>): MutableMap<Point2D, Char> = readBoard(lines) { it }

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
