#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 2000;

int a[maxn], b[maxn], d[maxn][maxn], p[maxn][maxn];

const int dx[3][2] = {-1, -1, 0, -1, -1, 0};

int main()
{
	ifstream cin("lcs.in");
	ofstream cout("lcs.out");

	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
		cin >> a[i];
	int m;
	cin >> m;
	for (int i = 0; i < m; ++i)
		cin >> b[i];

	d[0][0] = a[0] == b[0];
	p[0][0] = a[0] != b[0];
	for (int j = 1; j < m; ++j)
	{
		d[0][j] = d[0][j - 1];
		p[0][j] = 1;

		if ((a[0] == b[j]) > d[0][j])
		{
			d[0][j] = 1;
			p[0][j] = 0;
		}
	}

	for (int i = 1; i < n; ++i)
	{
		d[i][0] = d[i - 1][0];
		p[i][0] = 2;
		if ((a[i] == b[0]) > d[i][0])
		{
			d[i][0] = 1;
			p[i][0] = 0;
		}

		for (int j = 1; j < m; ++j)
		{
			d[i][j] = d[i - 1][j];
			p[i][j] = 2;

			if (d[i][j - 1] > d[i][j])
			{
				d[i][j] = d[i][j - 1];
				p[i][j] = 1;
			}

			if (a[i] == b[j] && d[i - 1][j - 1] + 1 > d[i][j])
			{
				d[i][j] = d[i - 1][j - 1] + 1;
				p[i][j] = 0;
			}
		}
	}

	//for (int i = 0; i < n; ++i)
	//{
	//	for (int j = 0; j < m; ++j)
	//		cerr << d[i][j] << " ";
	//	cerr << endl;
	//}
	//cerr << endl;
	//for (int i = 0; i < n; ++i)
	//{
	//	for (int j = 0; j < m; ++j)
	//		cerr << p[i][j] << " ";
	//	cerr << endl;
	//}

	int ansI = n - 1, ansJ = m - 1;
	vector<int> ans;
	while (ansI >= 0 && ansJ >= 0)
	{
		//cerr << ansI << " " << ansJ << endl;
		if (p[ansI][ansJ] == 0)
			ans.push_back(a[ansI]);

		int dxI = p[ansI][ansJ];
		ansI += dx[dxI][0];
		ansJ += dx[dxI][1];
	}

	reverse(ans.begin(), ans.end());

	cout << d[n - 1][m - 1] << "\n";
	for (int elem : ans)
		cout << elem << " ";

	return 0;
}
