#include <stdio.h>

int occurances(char *s, char c) {
	int n = 0;
	while (*s != '\0') {
		n += *s == c;
		++s;
	}
	return n;
}

int main()
{
	int silver = 0;
	int gold = 0;
	int range[2];
	char c;
	char pw[80];

	FILE *f = fopen("in2", "r");
	while (fscanf(f, "%d-%d %c: %s\n", &(range[0]), &(range[1]), &c, &pw) != EOF) {
		int occ = occurances(pw, c);
		silver += (occ >= range[0] && occ <= range[1]);
		gold += pw[range[0]-1] == c ^ pw[range[1]-1] == c;
	}

	printf("Silver: %d\n", silver);
	printf("Gold:   %d\n", gold);

	fclose(f);
	return 0;
}
