package pl.touk.dpr.aoc2020

object Util {
    fun getFileContent(fileName: String): String = javaClass.getResource(fileName).readText()
    fun getLinesFromFile(fileName: String): List<String> = getFileContent(fileName).lines()
    fun getNotEmptyLinesFromFile(fileName: String): List<String> = getLinesFromFile(fileName).filter { it.isNotEmpty() }
}