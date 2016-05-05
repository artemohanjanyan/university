#include "big_integer.h"

#include <climits>
#include <limits>
#include <cassert>

size_t big_integer::add(size_t a, size_t b, size_t &carry)
{
	__uint128_t x = a;
	x = x + b + carry;
	carry = static_cast<size_t>(x >> std::numeric_limits<size_t>::digits);
	return static_cast<size_t>(x);
}

size_t big_integer::sub(size_t a, size_t b, size_t &carry)
{
	__int128_t x = a;
	x = x - b - carry;
	carry = (x < 0);
	return static_cast<size_t>(x);
}

// ((hi << SIZE_T_BIT) | lo) == a * b
// (ignoring overflow)
size_t big_integer::mul(size_t a, size_t b, size_t &hi)
{
	__uint128_t x = a;
	x *= b;
	hi = static_cast<size_t>(x >> std::numeric_limits<size_t>::digits);
	return static_cast<size_t>(x);
}

size_t big_integer::div(size_t a1, size_t a2, size_t b)
{
	__uint128_t x = a1;
	x = ((x << std::numeric_limits<size_t>::digits) | a2);
	x /= b;
	assert(x < static_cast<__uint128_t>(std::numeric_limits<size_t>::max()));
	//return static_cast<size_t>(std::min(x, static_cast<__uint128_t>(std::numeric_limits<size_t>::max())));
	return static_cast<size_t>(x);
}
