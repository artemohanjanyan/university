#ifndef GEOMETRY
#define GEOMETRY

#include <glm/glm.hpp>

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

__int128_t determinant(std::array<std::array<int, 4>, 4> matrix)
{
	__int128_t result = 0;
	std::array<int, 4> permutation = {{0, 1, 2, 3}};
	int sign = 1;
	do
	{
		__int128_t product = 1;
		for (auto pair : boost::combine(matrix, permutation))
			product *= boost::get<0>(pair)[boost::get<1>(pair)];
		result += product * sign;
		sign *= -1;
	}
	while (boost::next_permutation(permutation));
	return result;
}

std::array<int, 4> convert(glm::ivec3 point)
{
	if (point.z == 0)
		return {{0, 0, 1, 0}};
	return {{point.x, point.y, point.x * point.x + point.y * point.y, 1}};
};

bool delaunay_criteria(std::array<glm::ivec3, 4> points)
{
	return determinant({{convert(points[0]), convert(points[1]), convert(points[2]), convert(points[3])}}) < 0;
}

#endif // GEOMETRY
