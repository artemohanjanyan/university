#ifndef CPP_NETWORK_STRING_BUFFER_H
#define CPP_NETWORK_STRING_BUFFER_H

#include <queue>
#include <string>

namespace network
{
	namespace utils
	{
		class string_buffer
		{
			std::queue<std::string> queue{};
			size_t read_n = 0;

		public:
			std::string const & get_str() const noexcept;

			size_t get_read_n() const noexcept;

			void pop(size_t read_n) noexcept;

			void push(std::string const &str);

			bool is_empty() const noexcept;
		};
	}
}

#endif //CPP_NETWORK_STRING_BUFFER_H
