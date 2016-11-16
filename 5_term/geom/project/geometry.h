#ifndef GEOMETRY
#define GEOMETRY

#include <glm/glm.hpp>

#include <boost/range/combine.hpp>
#include <boost/range/algorithm/permutation.hpp>

enum turn_t
{
	left, right, collinear
};

template <typename T>
constexpr T sign(T x)
{
	if (x < 0)
		return -1;
	if (x > 0)
		return 1;
	return 0;
}

turn_t turn(glm::ivec3 const &a, glm::ivec3 const &b, glm::ivec3 const &c)
{
	switch (sign((static_cast<int64_t>(b.x) * a.z - static_cast<int64_t>(a.x) * b.z) *
	             (static_cast<int64_t>(c.y) * a.z - static_cast<int64_t>(a.y) * c.z) -
	             (static_cast<int64_t>(b.y) * a.z - static_cast<int64_t>(a.y) * b.z) *
	             (static_cast<int64_t>(c.x) * a.z - static_cast<int64_t>(a.x) * c.z)))
	{
		case 1:
			return left;
		case 0:
			return collinear;
		case -1:
			return right;
		default:
			throw std::runtime_error("unexpected value");
	}
}

uint8_t ccw(uint8_t i) // Counterclockwise
{
	static constexpr std::array<uint8_t, 3> array = {{1, 2, 0}};
	return array[i];
}

uint8_t cw(uint8_t i) // Clockwise
{
	static constexpr std::array<uint8_t, 3> array = {{2, 0, 1}};
	return array[i];
}

bool contains(glm::ivec3 const &p,
              std::array<glm::ivec3, 3> triangle)
{
	assert(turn(triangle[0], triangle[1], triangle[2]) != right);

	for (uint8_t i = 0; i != 3; ++i)
		if (turn(triangle[i], triangle[ccw(i)], p) == right)
			return false;
	return true;
}

struct segment_t
{
	glm::ivec3 begin, end;
};

struct ray_t
{
	glm::ivec3 begin, end;
};

bool has_intersection(segment_t const &segment, ray_t const &ray)
{
	auto tmp = turn(ray.begin, ray.end, segment.begin);
	return tmp != turn(ray.begin, ray.end, segment.end) &&
	        tmp == turn(segment.end, segment.begin, ray.begin);
}

template<int n>
int64_t determinant(std::array<std::array<int, n>, n> const &matrix)
{
	int64_t det = 0;
	std::array<int, n> permutation;
	std::iota(permutation.begin(), permutation.end(), 0);
	do
	{
		int64_t product = 1;
		for (int i = 0; i < n; ++i)
			product *= matrix[i][permutation[i]];
		int inversions = 0;
		for (int i = 0; i < n; i++)
			for (int j = i + 1; j < n; j++)
				if (permutation[i] > permutation[j])
					inversions++;
		if (inversions % 2 == 1)
			product *= -1;
		det += product;
	}
	while (std::next_permutation(permutation.begin(), permutation.end()));
	return det;
}

std::array<int, 4> convert(glm::ivec3 point)
{
	if (point.z == 0)
		return {{0, 0, 1, 0}};
	return {{point.x, point.y, point.x * point.x + point.y * point.y, 1}};
}

bool need_to_flip(std::array<glm::ivec3, 4> points)
{
	int64_t det = determinant<4>({{convert(points[0]),
	                                  convert(points[1]),
	                                  convert(points[2]),
	                                  convert(points[3])}});
	return det > 0;
}

#endif // GEOMETRY
