#include "tests.h"
#include <iostream>
#include <cstddef>
#include <gmpxx.h>

using namespace std;

int main() {
	int test_id;
	cin >> test_id;
	pair<string, string> c = genTest(test_id);
	mpz_class a = mpz_class(c.first);
	mpz_class b = mpz_class(c.second);
	cout << mpz_class(a + b).get_str() << endl;
}
