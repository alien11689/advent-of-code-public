def text = new File('input.txt').text

List lines = text.split('\n')

List numbers = lines.collect {it.split('\t').collect {it as int}.sort().reverse() }

def findDivisible(List nums) {
	println nums
	int first = -1
	int second = -1
	for(int i = 0; i < nums.size() - 1; ++i){
		first = nums[i]
		for(int j = i + 1; j < nums.size(); ++j){
			if(first % nums[j] == 0) {
				second = nums[j]
				return first / second
			}
		}
	}
	return -1
}

println numbers.collect {findDivisible(it)}.sum()
