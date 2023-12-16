#include<stdio.h>
#include<string.h>
#include<stdlib.h>

// must be long enough to read the whole input string
#define INPUT_LENGTH 10240

int hash(char *txt, int length) {
	int res = 0;
	int i = 0;
	for (i = 0; i < length; ++i) {
		res = (res + txt[i]) * 17 % 256;
	}
	return res;
}

int part1(char *fileName) {
	FILE *f = fopen(fileName, "r");
	char buffer[INPUT_LENGTH];
	int result = 0;
	while (fgets(buffer, INPUT_LENGTH, f)) {
		char *token = strtok(buffer, ",");
		while (token != NULL) {
			int len = strlen(token);
			if (token[len - 1] == '\n') {
				len--;
			}
			int h = hash(token, len);
			result += h;
			token = strtok(NULL, ",");
		}
	}
	fclose(f);
	return result;
}

int part2(char *fileName) {
	return 0;
}

int main() {
	int result1 = part1("./input.txt");
	printf("Part 1\n%d\n", result1);
//  int result2 = part2("./input.txt");
//  printf("Part 2\n%d\n", result2);
	return 0;
}
