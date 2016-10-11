#include "string_view.h"

namespace utils
{
	string_view::string_view(char const *begin, char const *end) noexcept
			: begin_ptr{begin}
			, end_ptr{end}
	{}

	string_view::string_view(std::string::const_iterator begin, std::string::const_iterator end) noexcept
			: begin_ptr{&*begin}
			, end_ptr{&*end}
	{}

	string_view::string_view(char const *str) noexcept
			: begin_ptr{str}
	{
		for (auto it = str;; ++it)
			if (*it == 0)
			{
				end_ptr = it;
				break;
			}
	}

	string_view::string_view(std::string const &str) noexcept
			: string_view{str.begin(), str.end()}
	{}

	char const *string_view::begin() const noexcept
	{
		return begin_ptr;
	}

	char const *string_view::end() const noexcept
	{
		return end_ptr;
	}

	size_t string_view::size() const noexcept
	{
		return end_ptr - begin_ptr;
	}

	char string_view::operator[](size_t i) const noexcept
	{
		return begin_ptr[i];
	}
}