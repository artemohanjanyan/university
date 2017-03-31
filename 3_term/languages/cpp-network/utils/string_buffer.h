#ifndef CPP_NETWORK_STRING_BUFFER_H
#define CPP_NETWORK_STRING_BUFFER_H

#include "string_view.h"

#include <queue>
#include <string>

namespace utils
{
	class string_buffer
	{
		std::queue<std::string> queue_{};
		size_t read_n_ = 0;

	public:
		string_view top() const noexcept;

		void pop(size_t read_n) noexcept;

		void push(std::string const &str);

		bool is_empty() const noexcept;
	};
}

#endif //CPP_NETWORK_STRING_BUFFER_H
