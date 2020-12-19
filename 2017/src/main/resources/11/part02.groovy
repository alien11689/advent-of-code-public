String text = new File('input.txt').text.trim()

List processSteps = text.split(',').collect {it.trim()}

@groovy.transform.ToString
class Point {
        BigDecimal x
        BigDecimal y

        Point(x,y){
                this.x = x
                this.y = y
        }

        BigDecimal dist(){
                int dx
                int dy
                if(x < 0){
                        dx = x
                }else {
                        dx = x
                }
                if(y < 0){
                        dy = y
                }else{
                        dy = y
                }

                return (dx / 0.5 + dy - dx)
        }
}

Point winner = new Point(222.5, 536.5)

Point prev = new Point(0,0)
def points = []
def dest = processSteps.collect {step -> 
        switch(step) {
                case "n": prev = new Point(prev.x, prev.y -1); return prev
                case "ne": prev = new Point(prev.x + 0.5, prev.y - 0.5); return prev
                case "nw": prev = new Point(prev.x-0.5, prev.y -0.5); return prev
                case "s": prev = new Point(prev.x, prev.y +1); return prev
                case "se": prev = new Point(prev.x + 0.5, prev.y +0.5); return prev
                case "sw": prev = new Point(prev.x - 0.5, prev.y +0.5); return prev
        }
}.collect {it.dist()}.max()


println dest
