String text = new File('input.txt').text.trim()
int row = 25
int tall = 6

String black = '0'
String white = '1'
String transparent = '2'

List<String> layers = (text as List).collate(row * tall)

for (int high = 0; high < tall; ++high) {
    for (int r = 0; r < row; ++r) {
        String cur = layers.collect { it[high * row + r] }.find { it != transparent }
        print(cur == black ? ' ' : 'X')
    }
    println()
}