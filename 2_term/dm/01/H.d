import std.stdio;
import std.algorithm;

int[2][][] fullNet =
[
	[[1, 2], [3, 4], [5, 6], [7, 8], [9, 10], [11, 12], [13, 14], [15, 16]],

	[[1, 4], [2, 3], [5, 8], [6, 7], [9, 12], [10, 11], [13, 16], [14, 15]],
	[[1, 2], [3, 4], [5, 6], [7, 8], [9, 10], [11, 12], [13, 14], [15, 16]],

	[[1, 8], [2, 7], [3, 6], [4, 5], [9, 16], [10, 15], [11, 14], [12, 13]],
	[[1, 3], [2, 4], [5, 7], [6, 8], [9, 11], [10, 12], [13, 15], [14, 16]],
	[[1, 2], [3, 4], [5, 6], [7, 8], [9, 10], [11, 12], [13, 14], [15, 16]],

	[[1, 16], [2, 15], [3, 14], [4, 13], [5, 12], [6, 11], [7, 10], [8, 9]],
	[[1, 5], [2, 6], [3, 7], [4, 8], [9, 13], [10, 14], [11, 15], [12, 16]],
	[[1, 3], [2, 4], [5, 7], [6, 8], [9, 11], [10, 12], [13, 15], [14, 16]],
	[[1, 2], [3, 4], [5, 6], [7, 8], [9, 10], [11, 12], [13, 14], [15, 16]],
];

int main()
{
	File cin = stdin;
	File cout = stdout;
	//File cin = File("netbuild.in", "r");
	//File cout = File("netbuild.out", "w");

	int n;
	cin.readf(" %d", &n);
	int[2][][] net = new int[2][][10];
	int compN = 0;
	foreach (int i; 0..10)
	{
		int[2][] layer = new int[2][0];
		foreach (int[2] comp; fullNet[i])
			if (comp[0] <= n && comp[1] <= n)
			{
				++layer.length;
				layer[$ - 1] = comp;
			}

		net[i] = layer;
		compN += layer.length;
	}

	cout.writef("%d %d %d\n", n, compN, 10);
	foreach (int[2][] layer; net)
	{
		cout.writef("%d ", layer.length);
		foreach (int[2] comp; layer)
			cout.writef("%d %d ", comp[0], comp[1]);
		cout.writef("\n");
	}

	return 0;
}
