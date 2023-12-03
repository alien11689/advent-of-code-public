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

int part2(char *fileName) {
	FILE *f = fopen(fileName, "r");
	int length = 256;
	char buffer[256];
	char keys[] = "1234567890";
	char *words[10] = {
		"zero",
		"one",
		"two",
		"three",
		"four",
		"five",
		"six",
		"seven",
		"eight",
		"nine"
	};
	char reverseWords[10][10];
	int i;
	for (i = 0; i < 10; ++i) {
		strcpy(reverseWords[i], words[i]);
		strrev(reverseWords[i]);
	}
//  for (i = 0; i < 10; ++i) {
//      printf("Rev word of %s (%d) is %s\n", words[i], i, reverseWords[i]);
//  }
	int result = 0;
	while (fgets(buffer, length, f)) {
		int firstDigitIdx = strcspn(buffer, keys);
		char firstDigit = buffer[firstDigitIdx];
		int i;
		for (i = 0; i < 10; ++i) {
			char *foundWord = strstr(buffer, words[i]);
			if (foundWord) {
				int position = foundWord - buffer;
//              printf("%s -> %s at %ld\n", buffer, words[i], position);
				if (position < firstDigitIdx) {
					firstDigitIdx = position;
					firstDigit = '0' + i;
				}
			}
		}
		strrev(buffer);
		int lastDigitIdx = strcspn(buffer, keys);
		char lastDigit = buffer[lastDigitIdx];
//      printf("Checking %s", buffer);
		for (i = 0; i < 10; ++i) {
//          printf("Looking for %s (%i)\n", reverseWords[i], i);
			char *foundWord = strstr(buffer, reverseWords[i]);
			if (foundWord) {
				int position = foundWord - buffer;
//              printf("Found at %i\n", position);
				if (position < lastDigitIdx) {
					lastDigitIdx = position;
					lastDigit = '0' + i;
				}
			}
		}
		int val = (firstDigit - '0') * 10 + (lastDigit - '0');
//      printf("For %s num is %d\n", buffer, val);
		result += val;
	}
	fclose(f);
	return result;
}

int main() {
	int result1 = part1("./input.txt");
	printf("Part 1\n%d\n", result1);
	int result2 = part2("./input.txt");
	printf("Part 2\n%d\n", result2);
	return 0;
}
