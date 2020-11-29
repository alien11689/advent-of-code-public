package pl.touk.dpr.aoc2020

object Util {
    fun getFileContent(fileName: String) = javaClass.getResource(fileName).readText()
}