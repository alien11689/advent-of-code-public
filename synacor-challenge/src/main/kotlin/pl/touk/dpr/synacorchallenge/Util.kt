package pl.touk.dpr.synacorchallenge

import java.time.Duration

object Util {
    fun measureTime(r: Runnable) {
        val start = System.currentTimeMillis();
        r.run()
        val end = System.currentTimeMillis();
        println("Finished in ${Duration.ofMillis(end - start)}")
    }
}
