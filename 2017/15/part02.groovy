long genA = 883
long genB = 879
//	genA = 65
//	genB = 8921

int factorA = 16807
int factorB = 48271


int i = 0 
int judge = 0
List selectedA = []
List selectedB = []

int divA = 4
int divB = 8

div = 2147483647
def next(prev, factor){
	return prev * factor % div
}

def nextAccept(prev, factor, divider){
	int t = 1
	long nextV = next(prev, factor)
	while(nextV % divider != 0){
		nextV = next(nextV, factor)
	}
	return nextV
}

while( i < 5000000){
	genA = nextAccept(genA, factorA, divA)
	genB = nextAccept(genB, factorB, divB)
	int a = genA % 65536
	int b = genB % 65536
	if(a == b){++judge}
	++i
}

println judge
