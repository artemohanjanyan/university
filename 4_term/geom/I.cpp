#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

struct point
{
	int x, y;
};

int sqr(int x)
{
	return x * x;
}

int dist_squared(point a, point b)
{
	return sqr(a.x - b.x) + sqr(a.y - b.y);
}

enum class axis
{
	x, y
};

bool x_compare(point const &a, point const &b)
{
	return a.x < b.x || (a.x == b.x && a.y < b.y);
}

bool y_compare(point const &a, point const &b)
{
	return a.y < b.y || (a.y == b.y && a.x < b.x);
}

struct kd_tree
{
	struct node
	{
		int number_;
		point point_;
		axis sep_axis_;
		unique_ptr<node> left_, right_;

		void build(vector<pair<point, int>>::iterator begin,
		           vector<pair<point, int>>::iterator end)
		{
			auto point_comp = sep_axis_ == axis::x ? x_compare : y_compare;
			auto comp =
					[&](pair<point, int> const &a, pair<point, int> const &b) -> bool
					{
						return point_comp(a.first, b.first);
					};

			size_t n = end - begin;
			auto median_it = begin + n / 2;

			nth_element(begin, median_it, end, comp);
			number_ = median_it->second;
			point_ = median_it->first;

			axis next_axis = sep_axis_ == axis::x ? axis::y : axis::x;
			if (begin < median_it)
			{
				left_ = make_unique<node>();
				left_->sep_axis_ = next_axis;
				left_->build(begin, median_it);
			}
			++median_it;
			if (median_it < end)
			{
				right_ = make_unique<node>();
				right_->sep_axis_ = next_axis;
				right_->build(median_it, end);
			}
		}

		pair<point, int> nearest(pair<point, int> const &a, pair<point, int> const &b, point const &p)
		{
			if (dist_squared(p, a.first) < dist_squared(p, b.first))
				return a;
			else
				return b;
		}

		pair<point, int> nearest(point const &p)
		{
			if (left_ == nullptr)
				if (right_ == nullptr)
					return {point_, number_};
				else
					return nearest(right_->nearest(p), {point_, number_}, p);
			else
				if (right_ == nullptr)
					return nearest(left_->nearest(p), {point_, number_}, p);
				else
				{
					auto point_comp = sep_axis_ == axis::x ? x_compare : y_compare;
					int comp_res = point_comp(p, point_);

					pair<point, int> tmp_nearest =
							comp_res ? left_->nearest(p) : right_->nearest(p);

					int dist_to_sep_squared =
							sqr(sep_axis_ == axis::x ? p.x - point_.x : p.y - point_.y);

					if (dist_to_sep_squared < dist_squared(p, tmp_nearest.first))
						tmp_nearest = nearest(tmp_nearest, comp_res ? right_->nearest(p) : left_->nearest(p), p);

					return nearest(tmp_nearest, {point_, number_}, p);
				}
		}
	};

	unique_ptr<node> root_;

	kd_tree(vector<pair<point, int>> &points)
	{
		root_ = make_unique<node>();
		root_->sep_axis_ = axis::x;
		root_->build(points.begin(), points.end());
	}

	pair<point, int> nearest(point const &p)
	{
		return root_->nearest(p);
	}
};

int main()
{
	int n, m;
	cin >> n >> m;
	vector<pair<point, int>> points(n);
	for (int i = 0; i < n; ++i)
	{
		cin >> points[i].first.x >> points[i].first.y;
		points[i].second = i + 1;
	}

	kd_tree tree{points};

	for (int i = 0; i < m; ++i)
	{
		point p;
		cin >> p.x >> p.y;
		cout << tree.nearest(p).second << "\n";
	}

	return 0;
}
