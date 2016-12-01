String []  data = new File('day8.data').text.split('\n')

println data.collect {
    println it
    int l1 = groovy.json.StringEscapeUtils.escapeJavaScript(it)
        // " -> \"
        // \ -> \\
        //.replaceAll('"', '\\"')
        //.replaceAll("\\\\([^\\\\])", "\\\\\\\\$1")
        .size()
    l1 +2 - it.size()
}.sum()

