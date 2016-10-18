#include <iostream>
#include <vector>
#include <map>
#include <unordered_map>
#include <unordered_set>
#include <boost/range/adaptor/indexed.hpp>
#include <boost/range/algorithm/find_if.hpp>
#include <boost/algorithm/cxx11/any_of.hpp>
#include <glm/glm.hpp>

#include "geometry.h"

uint32_t const FAKE_ID = std::numeric_limits<uint32_t>::max();

struct vertex_t
{
	glm::ivec3 point;
	uint32_t triangle_id;
};

struct edge_t : public std::pair<uint32_t, uint32_t>
{
	edge_t(uint32_t const &a, uint32_t const &b)
			: pair(a, b)
	{}

	edge_t twin() const
	{
		return {second, first};
	}
};

segment_t edge_to_segment(edge_t const &edge, std::vector<vertex_t> const &vertices)
{
	return {vertices[edge.first].point, vertices[edge.second].point};
}

struct triangle_t
{
	std::array<uint32_t, 3> vertices;
	std::array<uint32_t, 3> neighbours = {{FAKE_ID, FAKE_ID, FAKE_ID}};

	edge_t edge(uint8_t i) const
	{
		return {vertices[ccw(i)], vertices[cw(i)]};
	}

	uint8_t vertex_id(uint32_t v) const
	{
		for (uint8_t i = 0; i < 3; ++i)
			if (vertices[i] == v)
				return i;
		throw std::runtime_error{"no such vertex"};
	}
};

bool contains(glm::ivec3 const &point,
              std::array<uint32_t, 3> const &triangle_vertices,
              std::vector<vertex_t> const &vertices)
{
	return contains(point, {vertices[triangle_vertices[0]].point,
	                        vertices[triangle_vertices[1]].point,
	                        vertices[triangle_vertices[2]].point});
}

uint8_t find_intersected_edge(triangle_t const &triangle, ray_t const &ray,
                              std::vector<vertex_t> const &vertices)
{
	assert(contains(ray.begin, triangle.vertices, vertices));
	for (uint8_t i = 0; i < 3; ++i)
		if (has_intersection(edge_to_segment(triangle.edge(i), vertices), ray))
			return i;
	throw std::runtime_error("doesn't intersect");
}

struct triangulation_t
{
	std::vector<vertex_t> vertices;
	std::vector<triangle_t> triangles;

	static constexpr uint32_t infinite_vertex_id = 0;
};

bool is_infinite(triangulation_t const &triangulation, uint32_t triangle_id)
{
	return boost::algorithm::any_of(triangulation.triangles[triangle_id].vertices,
	                                [](auto x)
	                                {
		                                return x == triangulation_t::infinite_vertex_id;
	                                });
}

std::tuple<uint32_t, uint8_t> mirror_edge(triangulation_t const &triangulation,
                                          uint32_t triangle_id,
                                          uint8_t edge_id)
{
	triangle_t const &triangle = triangulation.triangles[triangle_id];
	uint32_t vertex_id = triangle.vertices[ccw(edge_id)];
	triangle_id = triangle.neighbours[edge_id];
	return std::make_tuple(triangle_id, ccw(triangulation.triangles[triangle_id].vertex_id(vertex_id)));
}

struct triangle_cursor_t
{
	triangle_cursor_t(triangulation_t const &triangulation, uint32_t triangle_id)
			: triangulation_(&triangulation)
			, triangle_id_(triangle_id)
	{}

	uint8_t next(uint8_t edge_id)
	{
		uint8_t edge;
		std::tie(triangle_id_, edge) = mirror_edge(*triangulation_, triangle_id_, edge_id);
		return edge;
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

uint32_t find(glm::ivec3 const &point, triangulation_t const &triangulation)
{
	auto res = boost::find_if(triangulation.triangles, [&point, &triangulation](triangle_t const &triangle) {
		return contains(point, triangle.vertices, triangulation.vertices);
	});
	assert(res != triangulation.triangles.end());
	return static_cast<uint32_t>(std::distance(triangulation.triangles.begin(), res));
}

turn_t turn(std::array<uint32_t, 3> triangle_vertices, std::vector<vertex_t> const &vertices)
{
	return turn(vertices[triangle_vertices[0]].point,
	            vertices[triangle_vertices[1]].point,
	            vertices[triangle_vertices[2]].point);
}

bool check_triangle(triangulation_t const &triangulation, uint32_t triangle_id)
{
	auto const &triangle = triangulation.triangles[triangle_id];
	if (turn(triangle.vertices, triangulation.vertices) == right)
		return false;

	for (uint8_t edge_id = 0; edge_id < 3; ++edge_id)
	{
		auto mirror = mirror_edge(triangulation, triangle_id, edge_id);
		if (triangulation.triangles[std::get<0>(mirror)].neighbours[std::get<1>(mirror)] != triangle_id)
			return false;
	}

	return true;
}

void flip(triangulation_t &triangulation, uint32_t triangle_id, uint8_t edge_id)
{
	auto &triangle = triangulation.triangles[triangle_id];
	uint32_t adjacent_id;
	uint8_t adjacent_edge_id;
	std::tie(adjacent_id, adjacent_edge_id) = mirror_edge(triangulation, triangle_id, edge_id);
	auto &adjacent = triangulation.triangles[adjacent_id];

	{
		auto neighbour1 = mirror_edge(triangulation, triangle_id, cw(edge_id));
		auto neighbour2 = mirror_edge(triangulation, adjacent_id, cw(adjacent_edge_id));
		std::swap(triangulation.triangles[std::get<0>(neighbour1)].neighbours[std::get<1>(neighbour1)],
		          triangulation.triangles[std::get<0>(neighbour2)].neighbours[std::get<1>(neighbour2)]);
	}

	// Self-documenting code
	std::swap(triangle.neighbours[edge_id], adjacent.neighbours[cw(adjacent_edge_id)]);
	std::swap(adjacent.neighbours[adjacent_edge_id], triangle.neighbours[cw(edge_id)]);
	std::swap(triangle.neighbours[cw(edge_id)], adjacent.neighbours[cw(adjacent_edge_id)]);

	triangle.vertices[ccw(edge_id)] = adjacent.vertices[adjacent_edge_id];
	adjacent.vertices[ccw(adjacent_edge_id)] = triangle.vertices[edge_id];

	assert(check_triangle(triangulation, triangle_id));
	assert(check_triangle(triangulation, adjacent_id));
}

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
	uint32_t const inf_index = 0;
	triangulation.vertices.push_back({{0, 0, 0}, m});
	for (uint32_t i = 0; i < n; ++i)
	{
		int32_t x, y;
		std::cin >> x >> y >> tmp;
		triangulation.vertices.push_back({{x, y, 30000}, FAKE_ID});
	}

	std::map<edge_t, uint32_t> edge_to_triangle;
	for (uint32_t i = 0; i < m; ++i)
	{
		triangle_t triangle;

		std::cin >> tmp;
		for (auto &id : triangle.vertices)
		{
			std::cin >> id;
			++id;
		}
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

	// Traverse adjacent faces
	std::unordered_set<uint32_t> adjacent_ids;
	done = triangulation.vertices[v].triangle_id;
	triangle_cursor_t cursor{triangulation, done};
	uint8_t edge_start = cw(cursor.triangle().vertex_id(v));
	do
	{
		adjacent_ids.insert(cursor.triangle_id());
		edge_start = ccw(cursor.next(edge_start));
	}
	while (cursor.triangle_id() != done);

	// Find some infinite triangle
	uint32_t infinite_triangle = find({60000, 60000, 30000}, triangulation);

	// Ray
	ray_t ray = {{0, 0, 1},
	             {1, 1, 1}};
	std::unordered_set<uint32_t> intersected_ids;
	uint32_t first_triangle_id = find(ray.begin, triangulation);
	flip(triangulation, first_triangle_id, 0);
	first_triangle_id = find(ray.begin, triangulation);
	cursor = {triangulation, first_triangle_id};
	edge_start = find_intersected_edge(triangulation.triangles[first_triangle_id], ray, triangulation.vertices);
	do
	{
		intersected_ids.insert(cursor.triangle_id());
		uint8_t opposite = cursor.next(edge_start);
		// Assume that opposite vertex doesn't belong to ray
		if (has_intersection(
				edge_to_segment(cursor.triangle().edge(cw(opposite)), triangulation.vertices),
				ray))
			edge_start = cw(opposite);
		else
			edge_start = ccw(opposite);
	}
	while (cursor.triangle().vertices[0] != inf_index);

	std::cout << "OFF\n";
	std::cout << triangulation.vertices.size() << " " << triangulation.triangles.size() << " 0\n";
	for (auto const &vertex : triangulation.vertices)
		std::cout << vertex.point.x << " " << vertex.point.y << " " << vertex.point.z << "\n";
	for (auto const &triangle : triangulation.triangles | boost::adaptors::indexed{})
	{
		std::cout << "3 ";
		for (auto vertex : triangle.value().vertices)
			std::cout << vertex << " ";

		if (intersected_ids.count(static_cast<uint32_t>(triangle.index())))
			std::cout << "0 0 1 1";
		else if (triangle.index() == infinite_triangle)
			std::cout << "0 1 0 1";
		else if (adjacent_ids.count(static_cast<uint32_t>(triangle.index())))
			std::cout << "1 0 0 1";
		else
			std::cout << "0 0 0 1";
		std::cout << "\n";
	}

	return 0;
}
