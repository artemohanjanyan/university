import std.stdio;
import std.algorithm;

int binarySearch(int[] a, bool delegate(int x) predicate)
{
	int l = -1, r = cast(int) a.length;
	while (l + 1 < r)
	{
		int mid = l + (r - l) / 2;
		if (predicate(a[mid]))
			r = mid;
		else
			l = mid;
	}

	return r;
}

int main()
{
	File fin = File("binsearch.in", "r");
	File fout = File("binsearch.out", "w");
	//File fin = stdin;
	//File fout = stdout;

	int n;
	fin.readf(" %d", &n);
	int a[] = new int[n];
	for (int i = 0; i < n; ++i)
		fin.readf(" %d", &a[i]);

	int m;
	fin.readf(" %d", &m);
	for (int i = 0; i < m; ++i)
	{
		int q;
		fin.readf(" %d", &q);
		int l = binarySearch(a, x => q <= x), r = binarySearch(a, x => q < x) - 1;
		if (l == a.length || a[l] != q)
			fout.writeln("-1 -1");
		else
			fout.writeln(l + 1, " ", r + 1);
	}

	return 0;
}
