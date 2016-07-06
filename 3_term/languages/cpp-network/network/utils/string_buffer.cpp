#include "string_buffer.h"

namespace network
{
	namespace utils
	{
		std::string const &string_buffer::get_str() const noexcept
		{
			return queue.front();
		}

		size_t string_buffer::get_read_n() const noexcept
		{
			return read_n;
		}

		void string_buffer::pop(size_t read_n) noexcept
		{
			this->read_n += read_n;
			while (read_n >= queue.front().size())
				queue.pop();
			this->read_n = 0;
		}

		void string_buffer::push(std::string const &str)
		{
			queue.push(str);
		}

		bool string_buffer::is_empty() const noexcept
		{
			return queue.empty();
		}
	}
}