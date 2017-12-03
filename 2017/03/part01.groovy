def input = 277678
println "Search for $input"
// left 2, 11, 28, 
// diff 9, 17, 24
// side 3, 5, 7, 
// next side = s + 2
// next diff = (s - 1)/2 + 3*s + 3 + (s + 2 + 1)/2
// s = 3
def index = 1
def kw = 1
while (kw <= input) {
	index += 2
	kw = index * index

}
println "Lower right"
println index
println kw

println "Lower left"
def ll = (kw - index + 1)
println ll
println "Lower left is lower then search: ${ll < input}"
def halfSide =  (((index + 1) as int) / 2)
println "Half side = $halfSide"
def distSide = halfSide - 1
println "Dist side $distSide"
def middleSide = (kw - distSide)
println "Middle side $middleSide"
def leftOrRight = input - middleSide
println "Distance ${distSide + leftOrRight}"
println "476 is too high"
println "470 is too low"

