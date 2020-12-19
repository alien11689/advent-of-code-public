String []  data = new File('day8.data').text.split('\n')

println data.collect {
    println it
    int l1 = it.size()
    int l2 = it
    .replaceAll('\\\\\\\\', '1')
    .replaceAll(/\\"/, '1')
    .replaceAll(/\\x../, '1')
    .replaceAll('"', '')
    .size()
    l1 - l2
}.sum()

