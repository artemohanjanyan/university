#include <bits/stdc++.h>

using namespace std;

struct vec2d
{
	int x, y;
};

struct segment
{
	vec2d begin, end;
};

bool operator<(vec2d const &a, vec2d const &b)
{
	return a.x < b.x || (a.x == b.x && a.y < b.y);
}
bool operator==(vec2d const &a, vec2d const &b)
{
	return a.x == b.x && a.y == b.y;
}
bool operator<=(vec2d const &a, vec2d const &b)
{
	return a < b || a == b;
}
bool operator>(vec2d const &a, vec2d const &b)
{
	return a.x > b.x || (a.x == b.x && a.y > b.y);
}

int exterior_product(vec2d const &a, vec2d const &b, vec2d const &c)
{
	return (c.x - a.x) * (b.y - a.y) - (c.y - a.y) * (b.x - a.x);
}

int left_turn(vec2d const &a, vec2d const &b, vec2d const &c)
{
	int v1 = exterior_product(a, b, c);
	if (v1 > 0)
		return 1;
	if (v1 < 0)
		return -1;

	return 0;
}

bool between(vec2d x, vec2d l, vec2d r)
{
	return l <= x && x <= r;
}

//bool check(vec2d const &a, vec2d const &b, vec2d const &c, vec2d const &d)
bool check(segment const &s1, segment const &s2)
{
	auto &a = s1.begin, &b = s1.end, &c = s2.begin, &d = s2.end;

	int t1 = left_turn(a, b, c);
	int t2 = left_turn(a, b, d);
	int t3 = left_turn(c, d, a);
	int t4 = left_turn(c, d, b);

	if (t1 != t2 && t3 != t4)
		return true;
	else if (t1 == 0 && t2 == 0 && t3 == 0 && t4 == 0)
	{
		//if (a > b) swap(a, b);
		//if (c > d) swap(c, d);

		return between(c, a, b) || between(d, a, b) || between(a, c, d) || between(b, c, d);
	}
	else
		return false;
}

bool operator<(segment const &a, segment const &b)
{
	if (a.begin.y != b.begin.y)
		return a.begin.y < b.begin.y;
	if (a.begin.x != b.begin.x)
		return a.begin.x > b.begin.x;
	if (a.end.y != b.end.y)
		return a.end.y < b.end.y;
	return a.end.x < b.end.x;
}

int main()
{
	//ifstream cin("smoking.in");
	//ofstream cout("smoking.out");

	int n;
	cin >> n;
	vector<segment> segments;
	for (int i = 0; i < n; ++i)
	{
		segment seg;
		cin >> seg.begin.x;
		cin >> seg.begin.y;
		cin >> seg.end.x;
		cin >> seg.end.y;
		if (seg.begin.x > seg.end.x)
			swap(seg.begin, seg.end);
		segments.push_back(seg);
	}

	vector<pair<int, bool>> events(n * 2);
	for (int i = 0; i < n; ++i)
	{
		events[i].first = i;
		events[i].second = false;
	}
	for (int i = 0; i < n; ++i)
	{
		events[i + n].first = i;
		events[i + n].second = true;
	}

	sort(events.begin(), events.end(), [&](pair<int, bool> const &a, pair<int, bool> const &b)
			{
				int x1 = a.second ? segments[a.first].end.x : segments[a.first].begin.x;
				int x2 = b.second ? segments[b.first].end.x : segments[b.first].begin.x;
				return x1 < x2 || (x1 == x2 && a.second < b.second);
			});

	set<pair<segment, int>> status;

	for (auto &event : events)
	{
		set<pair<segment, int>>::iterator it;
		if (!event.second)
			it = status.insert(make_pair(segments[event.first], event.first)).first;
		else
			it = status.find(make_pair(segments[event.first], event.first));

		int i = -1;
		if (it != status.begin())
		{
			auto pit = prev(it);
			if (check(pit->first, it->first))
				i = pit->second;
		}
		auto nit = next(it);
		if (nit != status.end())
			if (check(it->first, nit->first))
				i = nit->second;

		if (i != -1)
		{
			cout << "YES\n";
			cout << i + 1 << " " << event.first + 1 << "\n";
			return 0;
		}

		if (event.second)
			status.erase(make_pair(segments[event.first], event.first));
	}

	cout << "NO\n";

	return 0;
}
