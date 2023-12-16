#include<stdio.h>
#include<string.h>
#include<stdbool.h>
#include<stdlib.h>

#define LINE_BUFFER_LENGTH 256

int part1(char *fileName) {
	FILE *f = fopen(fileName, "r");
	char buffer[LINE_BUFFER_LENGTH];
	int result = 0;
	while (fgets(buffer, LINE_BUFFER_LENGTH, f)) {
		char *gameHeader = strtok(strtok(buffer, "\n"), ":");
		char *rest = strtok(NULL, ":");
		strtok(gameHeader, " ");
		int gameId = atoi(strtok(NULL, " "));
		char *token;
		int curNum = -1;
		bool valid = true;
		while ((token = strsep(&rest, " ;,")) != NULL) {
			if (strlen(token) > 0) {
				if (curNum < 0) {
					curNum = atoi(token);
				} else {
					if (strcmp(token, "red") == 0) {
						if (curNum > 12) {
							valid = false;
							break;
						}
					} else if (strcmp(token, "green") == 0) {
						if (curNum > 13) {
							valid = false;
							break;
						}
					} else if (strcmp(token, "blue") == 0) {
						if (curNum > 14) {
							valid = false;
							break;
						}
					}
					curNum = -1;
				}
			}
		}
		if (valid) {
			result += gameId;
		}
	}
	fclose(f);
	return result;
}

int part2(char *fileName) {
	FILE *f = fopen(fileName, "r");
	char buffer[LINE_BUFFER_LENGTH];
	int result = 0;
	while (fgets(buffer, LINE_BUFFER_LENGTH, f)) {
		// TODO
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
