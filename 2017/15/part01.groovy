long genA = 883
long genB = 879
// genA = 65
// genB = 8921

int factorA = 16807
int factorB = 48271

int div = 2147483647

int i = 0 
int judge = 0
while( i < 40000000){
	genA = genA * factorA % div
	genB = genB * factorB % div
	String a = String.format("%32s", Integer.toBinaryString(genA as int)).replace(' ', '0').substring(16)
	String b = String.format("%32s", Integer.toBinaryString(genB as int)).replace(' ', '0').substring(16)
	if(a == b){++judge}
	++i
}
println judge
