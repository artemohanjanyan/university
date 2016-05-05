#ifndef BIG_INTEGER_H
#define BIG_INTEGER_H

#include <iosfwd>
#include <vector>

#include "digit_array.h"

struct big_integer
{
	big_integer();
	big_integer(big_integer const& other);
	big_integer(int a);
	explicit big_integer(std::string const& str);
	~big_integer();

	big_integer& operator=(big_integer const& other);

	big_integer& operator+=(big_integer const& rhs);
	big_integer& operator-=(big_integer const& rhs);
	big_integer& operator*=(big_integer const& rhs);
	big_integer& operator/=(big_integer rhs);
	big_integer& operator%=(big_integer const& rhs);

	big_integer& operator&=(big_integer const& rhs);
	big_integer& operator|=(big_integer const& rhs);
	big_integer& operator^=(big_integer const& rhs);

	big_integer& operator<<=(int rhs);
	big_integer& operator>>=(int rhs);

	big_integer operator+() const;
	big_integer operator-() const;
	big_integer operator~() const;

	big_integer& operator++();
	big_integer operator++(int);

	big_integer& operator--();
	big_integer operator--(int);

	friend bool operator==(big_integer const& a, big_integer const& b);
	friend bool operator!=(big_integer const& a, big_integer const& b);
	friend bool operator<(big_integer const& a, big_integer const& b);
	friend bool operator>(big_integer const& a, big_integer const& b);
	friend bool operator<=(big_integer const& a, big_integer const& b);
	friend bool operator>=(big_integer const& a, big_integer const& b);

	friend std::string to_string(big_integer a);

private:
	big_integer(size_t a);

	void pop_zeros();

	static size_t add(size_t a, size_t b, size_t &carry);
	static size_t sub(size_t a, size_t b, size_t &carry);
	static size_t mul(size_t a, size_t b, size_t &hi);
	static size_t div(size_t a1, size_t a2, size_t b);

	void add(big_integer const& other);
	bool sub(big_integer const& other);

	size_t get_digit(size_t i);
	static int words_to_bits(size_t n);

	bool is_zero() const;

	digit_array to_twos_complement(size_t length) const;
	static big_integer from_twos_complement(const digit_array& a);

	digit_array digits;
	static int const plus = 1;
	static int const minus = 0;
	int sign;
};

big_integer operator+(big_integer a, big_integer const& b);
big_integer operator-(big_integer a, big_integer const& b);
big_integer operator*(big_integer a, big_integer const& b);
big_integer operator/(big_integer a, big_integer const& b);
big_integer operator%(big_integer a, big_integer const& b);

big_integer operator&(big_integer a, big_integer const& b);
big_integer operator|(big_integer a, big_integer const& b);
big_integer operator^(big_integer a, big_integer const& b);

big_integer operator<<(big_integer a, int b);
big_integer operator>>(big_integer a, int b);

bool operator==(big_integer const& a, big_integer const& b);
bool operator!=(big_integer const& a, big_integer const& b);
bool operator<(big_integer const& a, big_integer const& b);
bool operator>(big_integer const& a, big_integer const& b);
bool operator<=(big_integer const& a, big_integer const& b);
bool operator>=(big_integer const& a, big_integer const& b);

std::string to_string(big_integer a);
std::ostream& operator<<(std::ostream& s, big_integer const& a);

#endif // BIG_INTEGER_H
