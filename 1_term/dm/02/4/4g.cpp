#include <bits/stdc++.h>
 
using namespace std;
 
const int maxn = 10;
vector<int> a[maxn];
int n, k;
 
void gen(int v, set<int> &left)
{
    if (left.empty())
    {
        if (v != k)
            return;
 
        for (int i = 0; i < v; ++i)
        {
            for (int j = 0; j < a[i].size(); ++j)
                cout << a[i][j] << " ";
            cout << endl;
        }
        cout << "\n";
 
        return;
    }
 
    if (v >= k)
        return;
 
    int first = *left.begin();
    left.erase(first);
 
    int endMask = (1 << left.size());
    for (int mask = 0; mask < endMask; ++mask)
    {
        a[v].clear();
        a[v].push_back(first);
 
        int maskLeft = mask;
        for (set<int>::iterator it = left.begin(); maskLeft != 0; maskLeft >>= 1)
            if (maskLeft & 1)
            {
                a[v].push_back(*it);
                left.erase(it++);
            }
            else
                ++it;
 
        gen(v + 1, left);
 
        for (int i = 1; i < a[v].size(); ++i)
            left.insert(a[v][i]);
    }
 
    left.insert(first);
}
 
int main()
{
    freopen("part2sets.in", "r", stdin);
    freopen("part2sets.out", "w", stdout);
 
    cin >> n >> k;
 
    set<int> numbers;
    for (int i = 0; i < n; ++i)
        numbers.insert(i + 1);
 
    gen(0, numbers);
 
    return 0;
}
