#include "string_buffer.h"

namespace utils
{
	string_view string_buffer::top() const noexcept
	{
		return string_view(queue_.front().begin() + read_n_, queue_.front().end());
	}

	void string_buffer::pop(size_t read_n) noexcept
	{
		this->read_n_ += read_n;
		while (queue_.size() > 0 && this->read_n_ >= queue_.front().size())
		{
			queue_.pop();
			this->read_n_ = 0;
		}
		this->read_n_ = 0;
	}

	void string_buffer::push(std::string const &str)
	{
		queue_.push(str);
	}

	bool string_buffer::is_empty() const noexcept
	{
		return queue_.empty();
	}
}