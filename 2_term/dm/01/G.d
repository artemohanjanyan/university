import std.stdio;
import std.algorithm;

void netSort(int[] a, int[2][][] net)
{
	foreach (int[2][] layer; net)
		foreach (int[2] comp; layer)
			if (a[comp[0]] > a[comp[1]])
				swap(a[comp[0]], a[comp[1]]);
}

int main()
{
	File cin = stdin;
	File cout = stdout;
	//File cin = File("netcheck.in", "r");
	//File cout = File("netcheck.out", "w");

	int n, m, k;
	cin.readf(" %d %d %d", &n, &m, &k);

	int[2][][] net = new int[2][][k];
	foreach (int i; 0..k)
	{
		int r;
		cin.readf(" %d", &r);
		net[i] = new int[2][r];
		foreach (int j; 0..r)
		{
			cin.readf(" %d %d", &net[i][j][0], &net[i][j][1]);
			--net[i][j][0];
			--net[i][j][1];
			net[i][j].sort;
		}
	}

	int lastMask = 1 << n;
	foreach (int mask; 0..lastMask)
	{
		int[] a = new int[n];
		int tmp = mask;
		foreach (int i; 0..n)
		{
			a[i] = (tmp & 1);
			tmp >>= 1;
		}
		int[] aCopy = new int[n];
		aCopy[] = a[];

		netSort(a, net);
		sort(aCopy);

		foreach (int i; 0..n)
			if (a[i] != aCopy[i])
			{
				cout.writeln("No");
				return 0;
			}
	}

	cout.writeln("Yes");
	
	return 0;
}
