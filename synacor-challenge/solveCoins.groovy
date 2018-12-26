int red = 2
int blue = 9
int corroded = 3
int concave = 7
int shiny = 5

println([red, blue, corroded, concave, shiny].permutations().find {
    it[0] + it[1] * it[2]**2 + it[3]**3 - it[4] == 399
})
