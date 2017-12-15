long genA = 883
long genB = 879
 genA = 65
 genB = 8921

int factorA = 16807
int factorB = 48271

int div = 2147483647

int i = 0 
while( i <= 5){
	genA = genA * factorA % div
	genB = genB * factorB % div
	String a = String.format("%032d", Integer.toBinaryString(genA as int))
	String b = String.format("%032d", Integer.toBinaryString(genB as int))
	println a
	println b
	println
	++i
}
