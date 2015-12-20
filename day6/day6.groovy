
String rules = new File('day6.data').text

List<Boolean> lights = (0..999).collect {
    (0..999).collect {
        false
    }
}
rules.split('\n').each{
    String [] parts = it.split(' ')
    if(it.startsWith('turn on')){
        def (startX,startY) = parts[2].split(',').collect {it as int}
        def (endX,endY) = parts[4].split(',').collect {it as int}
        (startX..endX).each{ x -> 
            (startY..endY).each {y ->
                lights[x][y] = true
            }
        }
    }else if (it.startsWith('turn off')){
        def (startX,startY) = parts[2].split(',').collect {it as int}
        def (endX,endY) = parts[4].split(',').collect {it as int}
        (startX..endX).each{ x -> 
            (startY..endY).each {y ->
                lights[x][y] = false
            }
        }
    }else {
        def (startX,startY) = parts[1].split(',').collect {it as int}
        def (endX,endY) = parts[3].split(',').collect {it as int}
        (startX..endX).each{ x -> 
            (startY..endY).each {y ->
                lights[x][y] = !lights[x][y]
            }
        }
    } 
}

println (lights.collectMany{ x -> 
    x.collect {y ->
        y ? 1 : 0
    }
}.sum())
