#include <bits/stdc++.h>
#include <boost/range/adaptor/indexed.hpp>

uint32_t const FAKE_ID = std::numeric_limits<uint32_t>::max();

struct vertex_t
{
	int32_t x, y, z;
	uint32_t triangle_id;
};

uint8_t cw(uint8_t i) // Clockwise
{
	static constexpr std::array<uint8_t, 3> array = {{1, 2, 0}};
	return array[i];
}

uint8_t ccw(uint8_t i) // Counterclockwise
{
	static constexpr std::array<uint8_t, 3> array = {{2, 0, 1}};
	return array[i];
}

struct edge_t : public std::pair<uint32_t, uint32_t>
{
	edge_t(uint32_t const &a, uint32_t const &b) : pair(a, b)
	{}

	edge_t twin() const
	{
		return {second, first};
	}
};

struct triangle_t
{
	std::array<uint32_t, 3> vertices;
	std::array<uint32_t, 3> neighbours = {{FAKE_ID, FAKE_ID, FAKE_ID}};

	edge_t edge(uint8_t i) const
	{
		return {vertices[cw(i)], vertices[ccw(i)]};
	}

	uint8_t vertex_id(uint32_t v) const
	{
		for (uint8_t i = 0; i < 3; ++i)
			if (vertices[i] == v)
				return i;
		throw std::runtime_error{"no such vertex"};
	}
};

struct triangulation_t
{
	std::vector<vertex_t> vertices;
	std::vector<triangle_t> triangles;
};

struct triangle_cursor_t
{
	triangle_cursor_t(triangulation_t const &triangulation, uint32_t triangle_id)
			: triangulation_(&triangulation)
			, triangle_id_(triangle_id)
	{}

	uint8_t next(uint8_t edge_id)
	{
		triangle_t const &triangle = triangulation_->triangles[triangle_id_];
		uint32_t vertex_id = triangle.vertices[cw(edge_id)];
		triangle_id_ = triangle.neighbours[edge_id];
		return cw(triangulation_->triangles[triangle_id_].vertex_id(vertex_id));
	}

	uint32_t triangle_id() const
	{
		return triangle_id_;
	}

	triangle_t const &triangle() const
	{
		return triangulation_->triangles[triangle_id_];
	}

private:
	triangulation_t const *triangulation_ = nullptr;
	uint32_t triangle_id_ = FAKE_ID;
};

int main(int argc, char *argv[])
{
	if (argc > 1)
		freopen(argv[1], "r", stdin);
	if (argc > 2)
		freopen(argv[2], "w", stdout);

	triangulation_t triangulation{};

	std::string tmp;
	std::cin >> tmp;

	uint32_t n, m;
	std::cin >> n >> m >> tmp;
	for (uint32_t i = 0; i < n; ++i)
	{
		int32_t x, y;
		std::cin >> x >> y >> tmp;
		triangulation.vertices.push_back({x, y, 30000, FAKE_ID});
	}
	uint32_t const inf_index = static_cast<uint32_t>(triangulation.vertices.size());
	triangulation.vertices.push_back({0, 0, 0, m});

	std::map<edge_t, uint32_t> edge_to_triangle;
	for (uint32_t i = 0; i < m; ++i)
	{
		triangle_t triangle;

		std::cin >> tmp;
		for (auto &id : triangle.vertices)
			std::cin >> id;
		for (auto &id : triangle.vertices)
			triangulation.vertices[id].triangle_id = i;

		for (uint8_t j = 0; j < 3; ++j)
			edge_to_triangle[triangle.edge(j)] = i;

		triangulation.triangles.push_back(triangle);
	}

	std::unordered_map<uint32_t, uint32_t> next_ch_vertex; // ch for convex hull
	for (uint32_t i = 0; i < m; ++i)
		for (uint8_t j = 0; j < 3; ++j)
		{
			auto edge = triangulation.triangles[i].edge(j);
			auto twin = edge.twin();

			if (edge_to_triangle.count(twin))
				triangulation.triangles[i].neighbours[j] = edge_to_triangle[twin];
			else
			{
				triangle_t triangle;
				triangle.vertices = {inf_index, twin.first, twin.second};
				triangle.neighbours[0] = i;

				next_ch_vertex[twin.first] = twin.second;
				uint32_t new_triangle_id = static_cast<uint32_t>(triangulation.triangles.size());
				edge_to_triangle[twin] = new_triangle_id;
				triangulation.triangles[i].neighbours[j] = new_triangle_id;

				triangulation.triangles.push_back(triangle);
			}
		}

	uint32_t v = next_ch_vertex.begin()->first;
	uint32_t done = v;
	do
	{
		uint32_t next = next_ch_vertex.at(v);

		uint32_t triangle_id = edge_to_triangle.at({v, next});
		uint32_t next_triangle_id = edge_to_triangle.at({next, next_ch_vertex.at(next)});

		assert(triangulation.triangles[triangle_id].vertices[2] == next);
		assert(triangulation.triangles[next_triangle_id].vertices[1] == next);

		triangulation.triangles[triangle_id].neighbours[1] = next_triangle_id;
		triangulation.triangles[next_triangle_id].neighbours[2] = triangle_id;

		v = next;
	}
	while (v != done);

	std::unordered_set<uint32_t> adjacent_ids;
	done = triangulation.vertices[v].triangle_id;
	triangle_cursor_t cursor{triangulation, done};
	uint8_t edge_start = ccw(cursor.triangle().vertex_id(v));
	do
	{
		adjacent_ids.insert(cursor.triangle_id());
		edge_start = cw(cursor.next(edge_start));
	}
	while (cursor.triangle_id() != done);

	std::cout << "OFF\n";
	std::cout << triangulation.vertices.size() << " " << triangulation.triangles.size() << " 0\n";
	for (auto const &vertex : triangulation.vertices)
		std::cout << vertex.x << " " << vertex.y << " " << vertex.z << "\n";
	for (auto const &triangle : triangulation.triangles | boost::adaptors::indexed{})
	{
		std::cout << "3 ";
		for (auto vertex : triangle.value().vertices)
			std::cout << vertex << " ";
		if (adjacent_ids.count(static_cast<uint32_t>(triangle.index())))
			std::cout << "1 0 0 1";
		else
			std::cout << "0 0 0 1";
		std::cout << "\n";
	}

	return 0;
}
