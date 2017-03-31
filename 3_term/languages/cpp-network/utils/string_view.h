#ifndef CPP_NETWORK_STRING_VIEW_H
#define CPP_NETWORK_STRING_VIEW_H

#include <stddef.h>

#include <string>

namespace utils
{
	class string_view
	{
		char const *begin_ptr_, *end_ptr_;

	public:
		string_view(char const *begin, char const *end) noexcept;

		string_view(std::string::const_iterator begin, std::string::const_iterator end) noexcept;

		string_view(char const *str) noexcept;

		string_view(std::string const &str) noexcept;

		char const *begin() const noexcept;

		char const *end() const noexcept;

		size_t size() const noexcept;

		char operator[](size_t i) const noexcept;
	};
}

#endif //CPP_NETWORK_STRING_VIEW_H
