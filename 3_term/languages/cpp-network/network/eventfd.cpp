#include "eventfd.h"
#include "network_exception.h"

#include <sys/eventfd.h>

namespace network
{
	eventfd::eventfd()
		: base_descriptor_resource{file_descriptor{check_return_code(::eventfd(0, 0))}}
	{}

	eventfd::eventfd(eventfd &&rhs) noexcept
		: base_descriptor_resource{std::move(rhs.fd)}
	{}

	uint64_t eventfd::read()
	{
		uint64_t value;
		check_return_code(
				eventfd_read(fd.get_raw_fd(), &value));
		return value;
	}

	void eventfd::write(uint64_t value)
	{
		check_return_code(
				eventfd_write(fd.get_raw_fd(), value));
	}
}
