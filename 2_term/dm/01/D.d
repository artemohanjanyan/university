import std.stdio;
import std.algorithm;

double binarySearch(double l, double r, bool delegate(double x) predicate)
{
	foreach (int i; 0..200)
	{
		double mid = (l + r) / 2;
		if (predicate(mid))
			r = mid;
		else
			l = mid;
	}

	return r;
}

int main()
{
	//File cin = stdin;
	//File cout = stdout;
	File cin = File("garland.in", "r");
	File cout = File("garland.out", "w");

	size_t n;
	double a;
	cin.readf(" %s %s", &n, &a);

	double[2][] x = new double[2][n];

	x[0..2] = [[0, a], [1.0, 0]];

	foreach (size_t i; 2..n)
		x[i] = 2 * x[i - 1][] - x[i - 2][] + [0.0, 2];

	double ans = binarySearch(0, 1000000000, delegate bool(double b)
	{
		double xValue = (b - x[$ - 1][1]) / x[$ - 1][0];
		double ans = a;
		foreach (ref double[2] height; x)
		{
			double[2] temp = height[] * [xValue, 1];
			ans = min(ans, reduce!"a + b"(temp[]));
		}
		return ans > 0;
	});

	cout.writef("%.2f\n", ans);
	
	return 0;
}
