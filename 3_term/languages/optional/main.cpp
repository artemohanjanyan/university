#include <bits/stdc++.h>

#include "optional.h"

using namespace std;

int main()
{
	optional<int> a(1);
	optional<int> b = a;
	if (a)
		cout << *a;
	if (b)
		cout << *b;

	optional<string> os;
	os.emplace("abc");
	cout << *os << endl;

	os = string{"absd"};
	cout << *os << endl;

	cerr << "\n\nqq\n";
	auto os1 = make_optional<string>("123");
	os1->erase(0, 1);
	cout << *os1 << "\n\n";

	cout << *make_optional<string>("abc") << endl;
	cout << (os <= os1);

	swap(os, os1);
	cout << *os << " " << *os1 << endl;

	// Assignment
	optional<string> fail;
	fail = make_optional<string>("abc");
	cout << *fail << endl;

	fail.clear();

	return 0;
}
