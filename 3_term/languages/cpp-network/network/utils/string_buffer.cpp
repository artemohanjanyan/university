#include "string_buffer.h"

namespace network
{
	namespace utils
	{
		string_view string_buffer::top() const noexcept
		{
			return string_view(queue.front().begin() + read_n, queue.front().end());
		}

		void string_buffer::pop(size_t read_n) noexcept
		{
			this->read_n += read_n;
			while (queue.size() > 0 && this->read_n >= queue.front().size())
			{
				queue.pop();
				this->read_n = 0;
			}
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