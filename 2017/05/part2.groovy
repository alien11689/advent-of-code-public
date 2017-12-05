def text = new File('input.txt').text

List steps = text.split('\n').collect  {it.trim() as int}

int cur = 0
int jumps = 0
while(cur < steps.size()){
	println "$cur/${steps.size()}"
	jumps++
	int val = steps[cur]
	steps[cur] = val >= 3 ? (val-1) : (val + 1)
	cur += val
}
println jumps
