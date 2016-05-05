#include <bits/stdc++.h>
using namespace std;

int main()
{
	auto f1 = [](double x, double y1, double y2){ return atan(x * x + y2 * y2); };
	auto f2 = [](double x, double y1, double y2){ return sin(x + y1); };
	double y1 = 0.5, y2 = 1.5, a = 0, b = 2, x = a;
	double const h = 0.01;

	do
	{
		cout << y1 << " " << y2 << endl;
		double tmp = y1;
		y1 = y1 + h * f1(x, y1, y2);
		y2 = y2 + h * f2(x, tmp, y2);
		x += h;
	} while (x <= b);

	return 0;
}
