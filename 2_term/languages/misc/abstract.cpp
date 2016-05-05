#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

struct A
{
	string str;
	virtual ~A() = 0;
	
	virtual void f()
	{
		cerr << "A.f\n";
	}
};

A::~A()
{
	f();
}

struct B : A
{
	~B()
	{
		cerr << "here\n";
	}

	virtual void f()
	{
		cerr << "B.f\n";
	}
};

int main()
{
	A* a = new B;
	delete a;
	return 0;
}
