String input = new File('day1.data').text

int floor = 0
int pos = 0
for(s in input){
    floor += (s == '(' ? 1 : -1)
    ++pos
    if(floor == -1){
        println pos
        break
    }
}
