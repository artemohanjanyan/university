import std.stdio;
import std.algorithm;

int[] genAntiQSort(int n, int function(int, int) pickMid)
{
	int[] perm = new int[n];
	foreach (int i; 0..n)
		perm[i] = i;

	foreach (int i; 0..n)
		swap(perm[i], perm[pickMid(i, n - 1)]);

	int[] a = new int[n];
	foreach (int i; 0..n)
		a[perm[i]] = i + 1;

	return a;
}

int main()
{
	//File cin = stdin;
	//File cout = stdout;
	File cin = File("antiqs.in", "r");
	File cout = File("antiqs.out", "w");
	
	int n;
	cin.readf(" %d", &n);
	int[] a = genAntiQSort(n, (a, b) => (a + b) / 2);
	foreach (int x; a)
		cout.write(x, " ");

	return 0;
}
