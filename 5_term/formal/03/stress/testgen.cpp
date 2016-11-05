#include <bits/stdc++.h>
using namespace std;

int main(int argc, char **argv)
{
	srand(atoi(argv[1]));

	string s = "ABCDEabcde";

	int m = rand() % 10 + 1;
	cout << m << " " << 'A' << "\n";
	for (int i = 0; i < m; ++i)
	{
		random_shuffle(s.begin(), s.end());
		cout << char(rand() % 5 + 'A') << " -> ";
		for (int j = 0; j < 4; ++j)
			if (rand() % 2)
				cout << s[j];
		cout << "\n";
	}

	return 0;
}
