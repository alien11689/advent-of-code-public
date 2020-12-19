import groovy.transform.*
String []  data = new File('day9.data').text.split('\n')

@ToString
class Path{
    Set cities
    int dist
}

paths = data.collect {
    def s = it.split(' ')
    new Path(cities: [s[0], s[2]] as Set, dist: s[4] as int )
}

def cities = (paths.collectMany {it.cities} as Set)

println cities
println paths

def findRoutes(Set cities){
    if(cities.size() == 1){
        return [[cities[0]]]
    }
    Stack stack = new Stack()
    stack.addAll(cities)
    List routes = []
    while (!stack.empty){
        String cur = stack.pop()
        println cur
        Set remainingCities = cities - cur
        List subRoutes = findRoutes(remainingCities)
        subRoutes.each {
            it << cur
        }
        routes << subRoutes.max {calculateDist(it)}
    }
    return routes
}

distMemory = [:]

def calculateDist(List<String> route){
    if(route in distMemory){
        return distMemory[route]
    }
    if(route.size() == 1){
        distMemory[route] = 0
    }else if (route.size() == 2){
        println route
        distMemory[route] = paths.find {(route as Set) == it.cities }.dist
    }else{
        distMemory[route] = calculateDist([route[0], route[1]]) + calculateDist(route[1..-1])
    }
    return distMemory[route]
}

def routes = findRoutes(cities)
println routes
println routes.max {calculateDist(it)}
println calculateDist(routes.max {calculateDist(it)})
