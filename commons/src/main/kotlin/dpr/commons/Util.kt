package dpr.commons

import java.time.Duration

object Util {
    @JvmStatic
    fun getFileContent(fileName: String): String = javaClass.getResource(fileName)!!.readText()

    @JvmStatic
    fun getFileContent(day: Int, fileName: String): String = javaClass.getResource("/%02d/%s".format(day, fileName))!!.readText()

    @JvmStatic
    fun getLinesFromFile(fileName: String): List<String> = getFileContent(fileName).lines()

    @JvmStatic
    fun getLinesFromFile(day: Int, fileName: String): List<String> = getFileContent(day, fileName).lines()

    @JvmStatic
    fun getNotEmptyLinesFromFile(fileName: String): List<String> = getLinesFromFile(fileName).filter { it.isNotEmpty() }

    @JvmStatic
    fun getNotEmptyLinesFromFile(day: Int, fileName: String): List<String> = getLinesFromFile(day, fileName).filter { it.isNotEmpty() }

    @JvmStatic
    fun <T> readBoard(lines: List<String>, valueMapper: (Char) -> T): MutableMap<Point2D, T> {
        val board = mutableMapOf<Point2D, T>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                board[Point2D(x, y)] = valueMapper(c)
            }
        }
        return board
    }

    @JvmStatic
    fun readBoard(lines: List<String>): MutableMap<Point2D, Char> = readBoard(lines) { it }

    @JvmStatic
    fun measureTime(r: Runnable) {
        val start = System.currentTimeMillis();
        r.run()
        val end = System.currentTimeMillis();
        println("Finished in ${Duration.ofMillis(end - start)}")
    }
}
