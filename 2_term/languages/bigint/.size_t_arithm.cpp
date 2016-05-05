#include "big_integer.h"

#include <climits>

// Even (sizeof(size_t) * CHAR_BIT)-specific code
// ---------------------------------------------------------
// TODO?: Preprocessor wrapper
//	Tags: constexpr, CHAR_BIT, SIZE_MAX

#define SIZE_T_BIT (sizeof(size_t) * CHAR_BIT)
#define SIZE_T_BIT2 (SIZE_T_BIT / 2)

// lo = a + b
// hi = carry
void big_integer::add(size_t a, size_t b, size_t &hi, size_t &lo)
{
	size_t ahi = (a >> SIZE_T_BIT2), alo = (a & ((size_t(1) << SIZE_T_BIT2) - 1));
	size_t bhi = (b >> SIZE_T_BIT2), blo = (b & ((size_t(1) << SIZE_T_BIT2) - 1));

	lo = a + b;
	hi = (((alo + blo) >> SIZE_T_BIT2) + ahi + bhi) >> SIZE_T_BIT2;
}

// ((hi << SIZE_T_BIT) | lo) == a * b
// (ignoring overflow)
void big_integer::mul(size_t a, size_t b, size_t &hi, size_t &lo)
{
	size_t ahi = (a >> SIZE_T_BIT2), alo = (a & ((size_t(1) << SIZE_T_BIT2) - 1));
	size_t bhi = (b >> SIZE_T_BIT2), blo = (b & ((size_t(1) << SIZE_T_BIT2) - 1));

	hi = 0;
	lo = alo * blo;
	add(lo, (blo * ahi) << SIZE_T_BIT2, hi, lo);
	add(lo, (bhi * alo) << SIZE_T_BIT2, hi, lo);
	hi += ahi * bhi + ((blo * ahi) >> SIZE_T_BIT2) + ((bhi * alo) >> SIZE_T_BIT2);
}
//End of even (sizeof(size_t) * CHAR_BIT)-specific code
