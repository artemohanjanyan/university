#ifndef OPTIONAL_H
#define OPTIONAL_H

#include <utility>
#include <stdexcept>

template <typename T>
class optional
{
private:
	union _u
	{
		T x;
		_u() {}
		~_u() {}
	} value;

	bool is_none;
	void dereference_exception()
	{
		if (is_none)
			throw std::runtime_error("dereference of none");
	}

public:
	optional(): is_none(true) {}

	optional(optional const& other)
	{
		is_none = other.is_none;
		if (!is_none)
			new(&value.x) T(other.value.x);
	}

	optional(optional&& other)
	{
		is_none = other.is_none;
		if (!is_none)
			new(&value.x) T(std::move(other.value.x));
	}

	optional(T const& other_value)
	{
		is_none = false;
		new(&this->value.x) T(other_value);
	}
	
	optional(T&& other_value)
	{
		is_none = false;
		new(&this->value.x) T(std::move(other_value));
	}

	void swap(optional& other)
	{
		using std::swap;
		if (*this && other)
			swap(value.x, other.value.x);
		else if (*this && !other)
		{
			new(&other.value.x) T(value.x);
			other.is_none = false;
			value.x.~T();
			is_none = true;
		}
		else if (!*this && other)
			other.swap(*this);
	}

	optional& operator=(optional const& other)
	{
		if (this == &other)
			return *this;
		if (*this)
			if (other)
				value.x = other.value.x;
			else
				value.x.~T();
		else
			if (other)
				new(&value.x) T(other.value.x);
		is_none = other.is_none;
		return *this;
	}

	optional& operator=(optional&& other)
	{
		if (*this)
			if (other)
				value.x = std::move(other.value.x);
			else
				value.x.~T();
		else
			if (other)
				new(&value.x) T(std::move(other.value.x));
		is_none = other.is_none;
		return *this;
	}

	~optional()
	{
		if (!is_none)
			value.x.~T();
	}

	void clear()
	{
		if (!is_none)
		{
			value.x.~T();
			is_none = true;
		}
	}

	T& operator*()
	{
		dereference_exception();
		return value.x;
	}

	T const& operator*() const
	{
		dereference_exception();
		return value.x;
	}

	T* operator->()
	{
		dereference_exception();
		return &value.x;
	}

	T const* operator->() const
	{
		dereference_exception();
		return &value.x;
	}

	template<typename ...Args>
	void emplace(Args&& ...args)
	{
		if (!is_none)
		{
			value.x.~T();
			is_none = true;
		}
		new(&value.x) T(std::forward<Args>(args)...);
		is_none = false;
	}

	explicit operator bool() const
	{
		return !is_none;
	}

	friend bool operator==(optional const& a, optional const& b)
	{
		if (a.is_none != b.is_none)
			return false;
		if (!a && !b)
			return true;
		return a.value.x == b.value.x;
	}

	friend bool operator!=(optional const& a, optional const& b)
	{
		return !(a == b);
	}

	friend bool operator<(optional const& a, optional const& b)
	{
		if (a.is_none != b.is_none)
			return a.is_none < b.is_none;
		if (!a && !b)
			return false;
		return a.value.x < b.value.x;
	}

	friend bool operator<=(optional const& a, optional const& b)
	{
		return a < b || a == b;
	}

	friend bool operator>(optional const& a, optional const& b)
	{
		return b < a;
	}

	friend bool operator>=(optional const& a, optional const& b)
	{
		return a > b || a == b;
	}
};

template<typename T, typename ...Args>
optional<T> make_optional(Args&& ...args)
{
	optional<T> tmp;
	tmp.emplace(std::forward<Args>(args)...);
	return std::move(tmp);
}

template<typename T>
void swap(optional<T>& a, optional<T>& b)
{
	a.swap(b);
}

#endif
