package dpr.eulerproject

import dpr.commons.Util

object Problem0059 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val code = Util.getFileContent("/p059_cipher.txt").trim()
            .split(",").map { it.toInt() }

        for (a in 'a'..'z') {
            for (b in 'a'..'z') {
                for (c in 'a'..'z') {
                    val key = listOf(a, b, c).map { it.code }
                    val decrypted = code.mapIndexed { i, cur -> cur.xor(key[i % key.size]).toChar() }.joinToString("")
                    if (decrypted.contains(" the ")) {
//                        println(decrypted)
                        println(decrypted.sumOf { it.code })
                        break
                    }
                }
            }
        }
    }
}
