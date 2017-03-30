#include "event_descriptor.h"
#include "network_exception.h"

#include <sys/eventfd.h>

namespace network
{
	event_descriptor::event_descriptor()
		: base_descriptor_resource{file_descriptor{check_return_code(::eventfd(0, 0))}}
	{}

	event_descriptor::event_descriptor(event_descriptor &&rhs) noexcept
		: base_descriptor_resource{std::move(rhs.fd)}
	{}

	uint64_t event_descriptor::read()
	{
		uint64_t value;
		check_return_code(
				eventfd_read(fd.get_raw_fd(), &value));
		return value;
	}

	void event_descriptor::write(uint64_t value)
	{
		check_return_code(
				eventfd_write(fd.get_raw_fd(), value));
	}
}
