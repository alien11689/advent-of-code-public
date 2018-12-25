def text = new File('input.txt').text

def lines = text.split('\n')


(0..<(lines.size() - 1)).each { i ->
    String a = lines[i]
    ((i + 1)..<(lines.size())).each { j ->
        String b = lines[j]
        int diff = 0
        for (int k = 0; k < a.size(); ++k) {
            if (a[k] != b[k]) {
                ++diff
                if (diff > 1) {
                    break
                }
            }
        }
        if (diff == 1) {
            String c = (0..<(a.size())).collect { a[it] == b[it] ? a[it] : "" }.join()
            println c
            throw new RuntimeException()
        }
    }
}
