#include<stdio.h>
#include<string.h>
#include<stdlib.h>

// must be long enough to read the whole input string
#define INPUT_LENGTH 10240
#define BOX_LENGTH 256
#define LENS_LENGTH 256

int hash(char *txt, int length) {
	int res = 0;
	int i = 0;
	for (i = 0; i < length; ++i) {
		res = (res + txt[i]) * 17 % BOX_LENGTH;
	}
	return res;
}

int part1(char *fileName) {
	FILE *f = fopen(fileName, "r");
	char buffer[INPUT_LENGTH];
	int result = 0;
	char *mem = NULL;
	while (fgets(buffer, INPUT_LENGTH, f)) {
		char *token = strtok_r(buffer, ",", &mem);
		while (token != NULL) {
			token = strtok(token, "\n");
			int len = strlen(token);
			int h = hash(token, len);
			result += h;
			token = strtok_r(NULL, ",", &mem);
		}
	}
	fclose(f);
	return result;
}

typedef struct Lens {
	char *name;
	int value;
} Lens;

typedef struct Box {
	int length;
	Lens lenses[LENS_LENGTH];
} Box;

int part2(char *fileName) {
	Box boxes[BOX_LENGTH];
	FILE *f = fopen(fileName, "r");
	char buffer[INPUT_LENGTH];
	int i;
	int j;
	for (i = 0; i < BOX_LENGTH; ++i) {
		boxes[i].length = 0;
	}
	while (fgets(buffer, INPUT_LENGTH, f)) {
		char *mem = NULL;
		char *token = strtok_r(buffer, ",", &mem);
		while (token != NULL) {
			token = strtok(token, "\n");
			int len = strlen(token);
//          printf("Checking '%s'\n", token);
			if (token[len - 1] == '-') {
//              printf("Removing lens\n");
				char *name = strtok(token, "-");
//              printf(" Name is %s\n", name);
				int h = hash(name, len - 1);
				int changed = 0;
				for (i = 0; i < boxes[h].length; ++i) {
//                  printf("Comparing %s with %s = %d\n",
//                         boxes[h].lenses[i].name, name,
//                         strcmp(boxes[h].lenses[i].name, name));
					if (strcmp(boxes[h].lenses[i].name, name) == 0) {
						free(boxes[h].lenses[i].name);
						changed = 1;
						break;
					}
				}
				if (changed) {
//                  printf("Really removing lens %s\n", name);
					while (i < boxes[h].length - 1) {
						boxes[h].lenses[i] = boxes[h].lenses[i + 1];
						++i;
					}
					boxes[h].length = boxes[h].length - 1;
				}
			} else {
				char *name = strtok(token, "=");
				int val = atoi(strtok(NULL, "="));
				int h = hash(name, len - 2);
				int changed = 0;
//              printf("Box %d has length %d\n", h, boxes[h].length);
				for (i = 0; i < boxes[h].length; ++i) {
//                  printf("Comparing %s with %s\n",
//                         boxes[h].lenses[i].name, name);
					if (strcmp(boxes[h].lenses[i].name, name) == 0) {
//                      printf("Changing lens\n");
						boxes[h].lenses[i].value = val;
						changed = 1;
						break;
					}
				}
				if (!changed) {
//                  printf("Adding lens %s\n", name);
					boxes[h].lenses[boxes[h].length].name = strdup(name);
					boxes[h].lenses[boxes[h].length].value = val;
					boxes[h].length = boxes[h].length + 1;
//                  printf("B length is %d\n", boxes[h].length);
				}
			}
			token = strtok_r(NULL, ",", &mem);
		}
	}
	fclose(f);
	int result = 0;
	for (i = 0; i < BOX_LENGTH; ++i) {
		for (j = 0; j < boxes[i].length; ++j) {
//          printf("Box %d has %d lenses\n", i, boxes[i].length);
			result += (i + 1) * (j + 1) * boxes[i].lenses[j].value;
			free(boxes[i].lenses[j].name);
		}
	}
	return result;
}

int main() {
	int result1 = part1("./input.txt");
	printf("Part 1\n%d\n", result1);
	int result2 = part2("./input.txt");
	printf("Part 2\n%d\n", result2);
	return 0;
}
