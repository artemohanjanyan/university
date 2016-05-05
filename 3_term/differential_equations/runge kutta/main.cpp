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

		double k1 = f1(x, y1, y2);
		double k2 = f1(x + h / 2, y1 + h * k1 / 2, y2 + h * k1 / 2);
		double k3 = f1(x + h / 2, y1 + h * k2 / 2, y2 + h * k2 / 2);
		double k4 = f1(x + h, y1 + h * k3, y2 + h * k3);
		y1 = y1 + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4);

		k1 = f2(x, tmp, y2);
		k2 = f2(x + h / 2, tmp + h * k1 / 2, y2 + h * k1 / 2);
		k3 = f2(x + h / 2, tmp + h * k2 / 2, y2 + h * k2 / 2);
		k4 = f2(x + h, tmp + h * k3, y2 + h * k3);
		y2 = y2 + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4);

		x += h;
	} while (x <= b);

	return 0;
}
