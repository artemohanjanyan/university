import std.stdio;

void bucketSort(T)(T[] a, size_t delegate(T a) index, size_t endIndex)
{
	size_t[] cnt = new size_t[endIndex];
	foreach (T elem; a)
		++cnt[index(elem)];

	T[][] buckets = new T[][endIndex];
	foreach (size_t i; 0..endIndex)
	{
		buckets[i] = new T[cnt[i]];
		cnt[i] = 0;
	}

	foreach (T elem; a)
	{
		size_t i = index(elem);
		buckets[i][cnt[i]++] = elem;
	}

	for (size_t i = 0, j = 0, bucketI = 0; i < a.length; ++i)
	{
		while (j == cnt[bucketI])
		{
			++bucketI;
			j = 0;
		}
		a[i] = buckets[bucketI][j++];
	}
}

int main()
{
	//File cin = stdin;
	//File cout = stdout;
	File cin = File("radixsort.in", "r");
	File cout = File("radixsort.out", "w");

	size_t n, m, k;
	cin.readf(" %s %s %s\n", &n, &m, &k);
	string[] a = new string[n];
	for (size_t i = 0; i < n; ++i)
		a[i] = cin.readln();

	for (size_t i = 0; i < k; ++i)
		bucketSort(a, (string s) => cast(size_t) s[m - i - 1], 256);

	foreach (string s; a)
		cout.write(s);

	return 0;
}
