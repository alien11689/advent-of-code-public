
String rules = new File('day6.data').text

List<Integer> lights = (0..999).collect {
    (0..999).collect {
        0
    }
}
rules.split('\n').each{
    String [] parts = it.split(' ')
    if(it.startsWith('turn on')){
        def (startX,startY) = parts[2].split(',').collect {it as int}
        def (endX,endY) = parts[4].split(',').collect {it as int}
        (startX..endX).each{ x -> 
            (startY..endY).each {y ->
                lights[x][y] += 1
            }
        }
    }else if (it.startsWith('turn off')){
        def (startX,startY) = parts[2].split(',').collect {it as int}
        def (endX,endY) = parts[4].split(',').collect {it as int}
        (startX..endX).each{ x -> 
            (startY..endY).each {y ->
                lights[x][y] -= 1
                if(lights[x][y] < 0){
                    lights[x][y] = 0
                }
            }
        }
    }else {
        def (startX,startY) = parts[1].split(',').collect {it as int}
        def (endX,endY) = parts[3].split(',').collect {it as int}
        (startX..endX).each{ x -> 
            (startY..endY).each {y ->
                lights[x][y] += 2
            }
        }
    } 
}

println (lights.collectMany{ x -> 
    x.collect {it }}.sum())
