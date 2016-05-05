import std.stdio;
import std.algorithm;
import std.random;

int kthElement(int[] a, int k)
{
	if (a.length == 1)
		return a[0];

	int mid = cast(int) uniform(0, a.length);
	int x = a[mid];
	int l = 0, r = cast(int) a.length - 1;

	while (l <= r)
	{
		while (a[l] < x)
			++l;
		while (a[r] > x)
			--r;

		if (l <= r)
			swap(a[l++], a[r--]);
	}

	if (r + 1 < l)
		--l;
	
	if (k < l)
		return kthElement(a[0..r + 1], k);
	else
		return kthElement(a[l..$], k - l);
}

int main()
{
	//File cin = stdin;
	//File cout = stdout;
	File cin = File("kth.in", "r");
	File cout = File("kth.out", "w");

	int n, k;
	cin.readf(" %d %d", &n, &k);
	int[] a = new int[n];
	int A, B, C;
	cin.readf(" %d %d %d %d %d", &A, &B, &C, &a[0], &a[1]);
	for (int i = 2; i < n; ++i)
		a[i] = A * a[i - 2] + B * a[i - 1] + C;

	cout.writef("%d\n", kthElement(a, k - 1));
	
	return 0;
}
