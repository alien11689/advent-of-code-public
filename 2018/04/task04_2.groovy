import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

def text = new File('sortedInput.txt').text
//def text = new File('otherInput.txt').text

def lines = text.split('\n')


def lastGuardId = null
LocalDateTime fall = null

def m = [:]

def asleep = 0
lines.each { line ->
    def parts = line.split('[ #]').findAll().collect { it.replaceAll(/\[/, '').replaceAll(/]/, '') }
    if (parts[2] == 'Guard') {
        lastGuardId = parts[3] as int
        m[lastGuardId] = m[lastGuardId] ?: []
        asleep = 0
    } else if (parts[2] == 'falls') {
        LocalDateTime ts = LocalDateTime.of(LocalDate.parse(parts[0]), LocalTime.parse(parts[1]))
        fall = ts
    } else {
        LocalDateTime ts = LocalDateTime.of(LocalDate.parse(parts[0]), LocalTime.parse(parts[1]))
        m[lastGuardId] << (fall.minute..<(ts.minute))
    }
}
int curMax = -1
int guardMax = -1
m.findAll { it.value }.collect { entry ->
    Map minMap = [:].withDefault { _ -> 0 }
    entry.value.each { Range r ->
        r.each {
            minMap[it]++
        }
    }
    [(entry.key): minMap.max { it.value }]
}.max {  }