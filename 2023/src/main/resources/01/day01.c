#include<stdio.h>
#include<string.h>
#include<stdlib.h>

void strrev(char *str) {
	int len = strlen(str);
	int i = 0;
	for (i = 0; i < len / 2; ++i) {
		char tmp = str[i];
		str[i] = str[len - 1 - i];
		str[len - 1 - i] = tmp;
	}
}

int part1(char *fileName) {
	FILE *f = fopen(fileName, "r");
	int length = 256;
	char buffer[256];
	char keys[] = "1234567890";
	int result = 0;
	while (fgets(buffer, length, f)) {
		int firstDigitIdx = strcspn(buffer, keys);
		char firstDigit = buffer[firstDigitIdx];
		strrev(buffer);
		int lastDigitIdx = strcspn(buffer, keys);
		char lastDigit = buffer[lastDigitIdx];
		result += (firstDigit - '0') * 10 + (lastDigit - '0');
	}
	fclose(f);
	return result;
}

int main() {
	int result1 = part1("./input.txt");
	printf("Part 1\n%d\n", result1);
	return 0;
}
