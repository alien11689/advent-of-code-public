
def text = new File('data').text

class Box{
	int l;
	int w;
	int h;

	int field(){
		return 2*l*w + 2*w*h + 2*h*l
	}

	int slack(){
		return [l*w,w*h,h*l].min()
	}
}

def boxes = text.split('\n').collect { def sizes = it.split('x'); new Box(l: sizes[0] as int , w:sizes[1] as int, h:sizes[2] as int)}
println(boxes.collect{ it.field() + it.slack()}.inject(0){a,b -> a+b})
