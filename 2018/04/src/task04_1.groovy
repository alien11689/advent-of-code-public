import groovy.transform.ToString

import java.time.Duration
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

def text = new File('sortedInput.txt').text
//def text = new File('otherInput.txt').text

def lines = text.split('\n')


def lastGuardId = null
def fall = null

def m = [:]

@ToString
class Sleep {
    int dur
    LocalTime start
    LocalTime end
    LocalDate date
}

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
        def dur = Duration.between(fall, ts).toMinutes()
        m[lastGuardId] << new Sleep(dur: dur, date: fall.toLocalDate(), start: fall.toLocalTime(), end: ts.toLocalTime())
    }
}

def max = m.max { it.value.dur.sum() }
println max.key
Map minutes = [:].withDefault { _ -> 0 }

max.value.each { Sleep s ->
    int start = s.start.minute
    int end = s.end.minute
    for (int i = 0; i < 60; ++i) {
        if (i >= start && i < end) {
            minutes[i] += 1
        }
    }
}
println(max.key * minutes.max { it.value }.key)
