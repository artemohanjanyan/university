#ifndef TESTS
#define TESTS

#include <utility>
#include <string>
#include <cassert>

using namespace std;

pair<string, string> genTest(int id) {
	const int N = 1;
	const string a[N] = {"2"};
	const string b[N] = {"3"};
	assert(0 <= id && id < N);
	return make_pair(a[id], b[id]);
}
#endif
