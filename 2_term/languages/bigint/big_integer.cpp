#include "big_integer.h"

#include <cstring>
#include <stdexcept>
#include <sstream>
#include <algorithm>
#include <limits>

big_integer::big_integer() : digits(0), sign(plus)
{}

big_integer::big_integer(big_integer const& other): digits(other.digits), sign(other.sign)
{}

// Assuming sizeof(int) <= sizeof(size_t)
big_integer::big_integer(int a) : digits(a == 0 ? 0 : 1, static_cast<size_t>(a)), sign(a >= 0 ? plus : minus)
{
	if (a < 0)
		digits[0] = -digits[0];
}

big_integer::big_integer(size_t a) : digits(a == 0 ? 0 : 1, a), sign(plus)
{}

big_integer::big_integer(std::string const& str) : big_integer()
{
	for (size_t i = (str[0] == '-' ? 1 : 0); i < str.length(); ++i)
	{
		*this *= 10;
		*this += str[i] - '0';
	}
	if (str[0] == '-')
		*this *= -1;
}

big_integer::~big_integer()
{}

big_integer& big_integer::operator=(big_integer const& other)
{
	digits = other.digits;
	sign = other.sign;
	return *this;
}

void big_integer::pop_zeros()
{
	while (digits.size() > 0 && digits.back() == 0)
		digits.pop_back();
}

void big_integer::add(big_integer const& other)
{
	if (digits.size() < other.digits.size())
		digits.resize(other.digits.size());

	size_t carry = 0;
	for (size_t i = 0; i < other.digits.size(); ++i)
		digits[i] = add(digits[i], other.digits[i], carry);
	for (size_t i = other.digits.size(); i < digits.size(); ++i)
		digits[i] = add(digits[i], 0, carry);

	if (carry > 0)
		digits.push_back(carry);	
}

// Returns |*this| < |other|
bool big_integer::sub(big_integer const& other)
{
	bool is_less = false;
	if (is_zero() && other.is_zero())
		is_less = false;
	else if (digits.size() != other.digits.size())
		is_less = digits.size() < other.digits.size();
	else
		for (size_t i = digits.size(); i > 0; --i)
			if (digits[i - 1] != other.digits[i - 1])
			{
				is_less = digits[i - 1] < other.digits[i - 1];
				break;
			}

	big_integer const *subtrahend = this, *subtractor = &other;

	if (is_less)
		std::swap(subtrahend, subtractor);

	if (subtrahend->digits.size() > digits.size())
		digits.resize(subtrahend->digits.size());

	size_t carry = 0;
	for (size_t i = 0; i < subtractor->digits.size(); ++i)
		digits[i] = sub(subtrahend->digits[i], subtractor->digits[i], carry);

	for (size_t i = subtractor->digits.size(); i < digits.size(); ++i)
		digits[i] = sub(subtrahend->digits[i], 0, carry);

	pop_zeros();

	return is_less;
}

big_integer& big_integer::operator+=(big_integer const& rhs)
{
	if (sign == rhs.sign)
		add(rhs);
	else
		sign ^= sub(rhs);

	return *this;
}

big_integer& big_integer::operator-=(big_integer const& rhs)
{
	if (sign == rhs.sign)
		sign ^= sub(rhs);
	else
		add(rhs);

	return *this;
}

big_integer& big_integer::operator*=(big_integer const& rhs)
{
	if (rhs.sign == minus)
		sign ^= 1;

	digits.resize(digits.size() + rhs.digits.size());

	for (size_t i = digits.size() - rhs.digits.size(); i > 0; --i)
	{
		size_t hi = 0, carry = 0, cur_digit = digits[i - 1];
		digits[i - 1] = 0;

		for (size_t j = 0; j < rhs.digits.size(); ++j)
		{
			digits[i - 1 + j] = add(digits[i - 1 + j], mul(cur_digit, rhs.digits[j], hi), carry);
			carry += hi;
		}

		for (size_t j = rhs.digits.size(); carry != 0; ++j)
			digits[i - 1 + j] = add(digits[i - 1 + j], 0, carry); 
	}

	pop_zeros();

	return *this;
}

size_t big_integer::get_digit(size_t i)
{
	if (i < digits.size())
		return digits[i];
	else
		return 0;
}

int big_integer::words_to_bits(size_t n)
{
	return static_cast<int>(std::numeric_limits<size_t>::digits * n);
}

big_integer& big_integer::operator/=(big_integer other)
{
	//if (other.is_zero())
	//	throw std::runtime_error("division by zero");

	int new_sign = sign;
	if (other.sign == minus)
		new_sign ^= 1;

	sign = other.sign = plus;

	if (*this < other)
		return *this = 0;

	int k = 0;
	size_t back_digit = other.digits.back();
	while (back_digit <= std::numeric_limits<size_t>::max() / 2)
	{
		++k;
		back_digit <<= 1;
	}

	*this <<= k;
	other <<= k;

	size_t m = digits.size() - other.digits.size();
	size_t n = other.digits.size();

	big_integer quotient;
	quotient.digits.resize(m + 1);

	if (*this >= (other << words_to_bits(m)))
	{
		quotient.digits[m] = 1;
		*this -= (other << words_to_bits(m));
	}

	for (size_t j = m; j > 0; --j)
	{
		quotient.digits[j - 1] = div(get_digit(n + j - 1), get_digit(n + j - 2), other.digits[n - 1]);
		*this -= quotient.digits[j - 1] * (other << words_to_bits(j - 1));
		while (*this < 0)
		{
			--quotient.digits[j - 1];
			*this += (other << words_to_bits(j - 1));
		}
	}

	quotient.sign = new_sign;
	quotient.pop_zeros();

	return *this = quotient;
}

big_integer& big_integer::operator%=(big_integer const& rhs)
{
	return *this -= (*this / rhs * rhs);
}

digit_array big_integer::to_twos_complement(size_t length) const
{
	digit_array vec = digits;
	vec.resize(length);

	if (sign == minus)
	{
		size_t carry = 0;
		for (size_t i = 0; i < vec.size(); ++i)
			vec[i] = add(~vec[i], i == 0, carry);
	}

	return vec;
}

big_integer big_integer::from_twos_complement(const digit_array& a)
{
	big_integer num;
	num.digits = a;

	if ((a.back() >> (std::numeric_limits<size_t>::digits - 1)) == 1)
	{
		size_t carry = 1;
		for (size_t i = 0; i < num.digits.size(); ++i)
			num.digits[i] = add(~num.digits[i], 0, carry);

		num.sign = minus;
	}

	num.pop_zeros();

	return num;
}

big_integer& big_integer::operator&=(big_integer const& rhs)
{
	size_t size = std::max(digits.size(), rhs.digits.size()) + 1;
	auto vec1 = to_twos_complement(size);
	auto vec2 = rhs.to_twos_complement(size);

	for (size_t i = 0; i < size; ++i)
		vec1[i] &= vec2[i];

	return *this = from_twos_complement(vec1);
}

big_integer& big_integer::operator|=(big_integer const& rhs)
{
	size_t size = std::max(digits.size(), rhs.digits.size()) + 1;
	auto vec1 = to_twos_complement(size);
	auto vec2 = rhs.to_twos_complement(size);

	for (size_t i = 0; i < size; ++i)
		vec1[i] |= vec2[i];

	return *this = from_twos_complement(vec1);
}

big_integer& big_integer::operator^=(big_integer const& rhs)
{
	size_t size = std::max(digits.size(), rhs.digits.size()) + 1;
	auto vec1 = to_twos_complement(size);
	auto vec2 = rhs.to_twos_complement(size);

	for (size_t i = 0; i < size; ++i)
		vec1[i] ^= vec2[i];

	return *this = from_twos_complement(vec1);
}

big_integer& big_integer::operator<<=(int rhs)
{
	size_t shift_n = static_cast<size_t>(rhs / std::numeric_limits<size_t>::digits); // word_shift_n
	auto vector = to_twos_complement(digits.size() + shift_n + 1);

	for (size_t i = digits.size(); i > 0; --i)
		vector[i + shift_n - 1] = vector[i - 1];
	for (size_t i = 0; i < shift_n; ++i)
		vector[i] = 0;

	shift_n = rhs - shift_n * std::numeric_limits<size_t>::digits; // bit_shift_n

	if (shift_n != 0)
	{
		for (size_t i = vector.size(); i > 1; --i)
			vector[i - 1] = ((vector[i - 1] << shift_n) | (vector[i - 2] >> (std::numeric_limits<size_t>::digits - shift_n)));
		vector[0] <<= shift_n;
	}

	return *this = from_twos_complement(vector);
}

big_integer& big_integer::operator>>=(int rhs)
{
	size_t shift_n = static_cast<size_t>(rhs / std::numeric_limits<size_t>::digits); // word_shift_n
	auto vector = to_twos_complement(digits.size() + 1);
	size_t pad = vector.back();

	for (size_t i = 0; i < digits.size() - shift_n; ++i)
		vector[i] = vector[i + shift_n];
	for (size_t i = digits.size() - shift_n; i < vector.size(); ++i)
		vector[i] = pad;

	shift_n = rhs - shift_n * std::numeric_limits<size_t>::digits; // bit_shift_n

	if (shift_n != 0)
	{
		for (size_t i = 0; i < vector.size() - 1; ++i)
			vector[i] = ((vector[i] >> shift_n) | (vector[i + 1] << (std::numeric_limits<size_t>::digits - shift_n)));
		vector.back() = pad;
	}

	return *this = from_twos_complement(vector);
}

big_integer big_integer::operator+() const
{
	return *this;
}

big_integer big_integer::operator-() const
{
	return *this * -1;
}

big_integer big_integer::operator~() const
{
	auto vec = to_twos_complement(digits.size() + 1);

	for (size_t i = 0; i < vec.size(); ++i)
		vec[i] = ~vec[i];

	return from_twos_complement(vec);
}

big_integer& big_integer::operator++()
{
	return *this += 1;
}

big_integer big_integer::operator++(int)
{
	big_integer const r = *this;
	++*this;
	return r;
}

big_integer& big_integer::operator--()
{
	return *this -= 1;
}

big_integer big_integer::operator--(int)
{
	big_integer const r = *this;
	--*this;
	return r;
}

big_integer operator+(big_integer a, big_integer const& b)
{
	return a += b;
}

big_integer operator-(big_integer a, big_integer const& b)
{
	return a -= b;
}

big_integer operator*(big_integer a, big_integer const& b)
{
	return a *= b;
}

big_integer operator/(big_integer a, big_integer const& b)
{
	return a /= b;
}

big_integer operator%(big_integer a, big_integer const& b)
{
	return a %= b;
}

big_integer operator&(big_integer a, big_integer const& b)
{
	return a &= b;
}

big_integer operator|(big_integer a, big_integer const& b)
{
	return a |= b;
}

big_integer operator^(big_integer a, big_integer const& b)
{
	return a ^= b;
}

big_integer operator<<(big_integer a, int b)
{
	return a <<= b;
}

big_integer operator>>(big_integer a, int b)
{
	return a >>= b;
}

bool big_integer::is_zero() const
{
	return digits.size() == 0;
}

bool operator==(big_integer const& a, big_integer const& b)
{
	if (a.is_zero() && b.is_zero())
		return true;
	return a.sign == b.sign && a.digits == b.digits;
}

bool operator!=(big_integer const& a, big_integer const& b)
{
	return !(a == b);
}

bool operator<(big_integer const& a, big_integer const& b)
{
	if (a.is_zero() && b.is_zero())
		return false;

	if (a.sign != b.sign)
		return a.sign < b.sign;

	if (a.digits.size() != b.digits.size())
		return a.digits.size() < b.digits.size();

	for (size_t i = a.digits.size(); i > 0; --i)
		if (a.digits[i - 1] != b.digits[i - 1])
			return a.digits[i - 1] < b.digits[i - 1];

	return false;
}

bool operator>(big_integer const& a, big_integer const& b)
{
	return !(a <= b);
}

bool operator<=(big_integer const& a, big_integer const& b)
{
	if (a.is_zero() && b.is_zero())
		return true;

	if (a.sign != b.sign)
		return a.sign <= b.sign;

	if (a.digits.size() != b.digits.size())
		return a.digits.size() <= b.digits.size();

	for (size_t i = a.digits.size(); i > 0; --i)
		if (a.digits[i - 1] != b.digits[i - 1])
			return a.digits[i - 1] <= b.digits[i - 1];

	return true;
}

bool operator>=(big_integer const& a, big_integer const& b)
{
	return !(a < b);
}

std::string to_string(big_integer a)
{
	if (a.is_zero())
		return "0";

	bool is_negative = (a.sign == big_integer::minus);
	if (is_negative)
		a *= -1;

	std::string str;
	while (a != 0)
	{
		big_integer const mod = a % 10;
		if (mod != 0)
			str.append(1, char(mod.digits[0] + '0'));
		else
			str.append(1, '0');
		a /= 10;
	}

	if (is_negative)
		str.append("-");

	std::reverse(str.begin(), str.end());

	return str;
}

std::ostream& operator<<(std::ostream& s, big_integer const& a)
{
	return s << to_string(a);
}
