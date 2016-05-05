#include <bits/stdc++.h>

#include "big_integer.h"
#include "digit_array.h"

using namespace std;

constexpr size_t bits(unsigned int c)
{
	return c == 0 ? 0 : 1 + bits(c >> 1);
}

constexpr size_t byte_size()
{
	return bits(~0) / sizeof(unsigned int);
}

int main()
{
	//big_integer a("10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
	//big_integer b(                                                     "100000000000000000000000000000000000000");
	//big_integer c("100000000000000000000000000000000000000000000000000000");

	//cout << a / b;


	big_integer a = 5;
	big_integer b = 20;

	big_integer q = a * b;

	cout << q << endl;
	cout << (q == 100) << endl;
	cout << endl;

	a *= b;

	cout << a << endl;
	cout << (a == 100) << endl;
	cout << endl;

	cout << sizeof(big_integer);

	return 0;
}
