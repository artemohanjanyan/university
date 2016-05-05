#include <bits/stdc++.h>

using namespace std;

struct base
{
	virtual ~base()
	{}

	virtual char const* msg() const
	{
		return "base";
	}
};

struct derived : base
{
	virtual ~derived()
	{}

	virtual char const* msg() const
	{
		return "derived";
	}
};

int main()
{
	cerr << "qq\n";
	base b;
	cerr << b.msg();
	return 0;
}
