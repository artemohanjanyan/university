import std.stdio;
import std.algorithm;

int size;
int[] a;

void siftDown(int i)
{
	while (i * 2 + 1 < size)
	{
		int left = i * 2 + 1;
		int right = i * 2 + 2;

		int j = left;
		if (right < size && a[left] < a[right])
			j = right;

		if (a[i] >= a[j])
			break;

		swap(a[i], a[j]);
		i = j;
	}
}

int pop()
{
	swap(a[0], a[--size]);
	siftDown(0);
	return a[size];
}

int main()
{
	auto fin = File("sort.in", "r");
	auto fout = File("sort.out", "w");

	int n;
	fin.readf(" %d", &n);
	a = new int[n];
	for (int i = 0; i < n; ++i)
		fin.readf(" %d", &a[i]);

	size = n;
	for (int i = n / 2 - 1; i >= 0; --i)
		siftDown(i);

	for (int i = 0; i < n; ++i)
		pop();

	foreach (int x; a)
		fout.write(x, " ");

	return 0;
}
