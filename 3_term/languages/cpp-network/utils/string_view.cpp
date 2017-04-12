#include "string_view.h"

namespace utils
{
	string_view::string_view(char const *begin, char const *end) noexcept
			: begin_ptr_{begin}
			, end_ptr_{end}
	{}

	string_view::string_view(std::string::const_iterator begin, std::string::const_iterator end) noexcept
			: begin_ptr_{&*begin}
			, end_ptr_{&*end}
	{}

	string_view::string_view(char const *str) noexcept
			: begin_ptr_{str}
	{
		for (auto it = str; ; ++it)
			if (*it == 0)
			{
				end_ptr_ = it;
				break;
			}
	}

	string_view::string_view(std::string const &str) noexcept
			: string_view{str.cbegin(), str.cend()}
	{}

	char const *string_view::begin() const noexcept
	{
		return begin_ptr_;
	}

	char const *string_view::end() const noexcept
	{
		return end_ptr_;
	}

	size_t string_view::size() const noexcept
	{
		return end_ptr_ - begin_ptr_;
	}

	char string_view::operator[](size_t i) const noexcept
	{
		return begin_ptr_[i];
	}
}