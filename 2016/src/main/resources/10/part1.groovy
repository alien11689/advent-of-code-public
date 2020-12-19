def main(String input) {
    List<String> lines = new File(input).text
        .split('\n')
    Map<String, Object> bots = lines.findAll {it.startsWith 'bot'}
        .collectEntries {
            String [] split = it.split(/ /)
            String botName = "${split[0]}_${split[1]}"
            String lower = "${split[5]}_${split[6]}"
            String upper = "${split[10]}_${split[11]}"
            Bot bot = new Bot(
                name: botName,
                lower: lower,
                upper: upper
            )
            [(botName): bot]
        }
    lines.findAll {it.startsWith 'value'}
        .each {
            String [] split = it.split(/ /)
            def bot = bots["${split[4]}_${split[5]}" as String]
            bot.values << (split[1] as int)
        }
    while(!bots.every {it.value.used()}){
        bots.keySet().findAll {it.startsWith("bot")}
            .collect {bots[it]}
            .findAll {it.ready() && !it.used()}
            .each {
                it.pass(bots)
            }
    }
}

@groovy.transform.ToString
class Bot {
    String name
    List<Integer> values = []
    String lower
    String upper
    Set<Integer> compared = []

    boolean ready(){
        values.size() == 2
    }
    boolean used(){!compared.isEmpty()}

    def pass(Map bots){
        compared.addAll values
        if(compared == ([61, 17] as Set)){
            println name
        }
        def lowerDest = bots[lower]?: new Out()
        def upperDest = bots[upper]?: new Out()
        lowerDest.values << values.min()
        upperDest.values << values.max()
    }
}

@groovy.transform.ToString
class Out {
    List<Integer> values = []
    boolean ready(){true}
    boolean used(){true}
}


println(main('sample.txt'))
println()
println(main('input.txt'))
