#include <bits/stdc++.h>
using namespace std;

struct A
{
	virtual char const* f() const
	{
		return "A";
	}
};

struct B : A
{
	virtual char const* f() const
	{
		return "B";
	}
};

int main()
{
	try
	{
		throw B();
	}
	catch (A a)
	{
		cout << a.f();
	}

	return 0;
}
