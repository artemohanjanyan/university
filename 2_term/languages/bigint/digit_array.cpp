#include "digit_array.h"

#include <algorithm>

digit_array::digit_array(size_t size, size_t init) :
		element_n(size)
{
	if (is_small())
		digit = init;
	else
		vec = new shared_vector(size, init);
}

digit_array::digit_array(digit_array const& other) :
		element_n(other.element_n)
{
	if (is_small())
		digit = other.digit;
	else
		vec = new shared_vector(*other.vec);
}

digit_array::~digit_array()
{
	if (!is_small())
		delete vec;
}

digit_array& digit_array::operator=(digit_array const& other)
{
	if (&other == this)
		return *this;

	if (!is_small())
		delete vec;

	element_n = other.element_n;
	if (is_small())
		digit = other.digit;
	else
		vec = new shared_vector(*other.vec);

	return *this;
}

void digit_array::resize(size_t new_size)
{
	if (new_size <= size())
		return;

	if (new_size == 1)
	{
		element_n = 1;
		digit = 0;
	}
	else if (is_small())
	{
		size_t temp = digit;

		vec = new shared_vector(new_size, 0);
		if (element_n == 1)
			(*vec)->at(0) = temp;

		element_n = new_size;
	}
	else
	{
		vec->detach();
		(*vec)->resize(new_size);
	}
}

size_t digit_array::size() const
{
	if (is_small())
		return element_n;
	else
		return (*vec)->size();
}

void digit_array::push_back(size_t x)
{
	resize(size() + 1);
	back() = x;
}

void digit_array::pop_back()
{
	if (!is_small())
	{
		vec->detach();
		(*vec)->pop_back();
	}
	else
		--element_n;
}

bool operator==(digit_array const& a, digit_array const& b)
{
	if (a.size() != b.size())
		return false;
	for (size_t i = 0; i < a.size(); ++i)
		if (a[i] != b[i])
			return false;
	return true;
}

bool operator!=(digit_array const& a, digit_array const& b)
{
	return !(a == b);
}
