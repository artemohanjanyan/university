#ifndef DIGIT_ARRAY_H
#define DIGIT_ARRAY_H

#include <cstddef>
#include <vector>
#include <memory>

//#include <vector>
//using digit_array = std::vector<size_t>;
//
//#include <iostream>

struct digit_array
{
	digit_array(size_t size = 0, size_t init = 0);
	digit_array(digit_array const& other);
	~digit_array();

	digit_array& operator=(digit_array const& other);

	void resize(size_t new_size);
	size_t size() const;

	size_t& operator[](size_t i);
	size_t operator[](size_t i) const;
	size_t& back();
	size_t back() const;

	void push_back(size_t x);
	void pop_back();

	friend bool operator==(digit_array const& a, digit_array const& b);
	friend bool operator!=(digit_array const& a, digit_array const& b);

private:
	size_t element_n;
	bool is_small() const;

	struct shared_vector : public std::shared_ptr<std::vector<size_t>>
	{
		using shared_ptr::shared_ptr;

		shared_vector(size_t size = 0, size_t init = 0);

		void detach();
	};

	union
	{
		size_t digit;
		shared_vector* vec;
	};
};

inline bool digit_array::is_small() const
{
	return element_n <= 1;
}

inline size_t& digit_array::operator[](size_t i)
{
	if (is_small())
		return digit;
	else
	{
		vec->detach();
		return (*vec)->at(i);
	}
}

inline size_t digit_array::operator[](size_t i) const
{
	if (is_small())
		return digit;
	else
		return (*vec)->at(i);
}

inline size_t& digit_array::back()
{
	if (is_small())
		return digit;
	else
	{
		vec->detach();
		return (*vec)->back();
	}
}

inline size_t digit_array::back() const
{
	if (is_small())
		return digit;
	else
		return (*vec)->back();
}

inline digit_array::shared_vector::shared_vector(size_t size, size_t init) :
		shared_vector(std::make_shared<std::vector<size_t>>(size, init))
{}

inline void digit_array::shared_vector::detach()
{
	if (use_count() > 1)
		*this = std::make_shared<std::vector<size_t>>(*(*this));
}

#endif // DIGIT_ARRAY_H
