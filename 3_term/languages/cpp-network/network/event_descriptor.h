#ifndef CPP_NETWORK_EVENTFD_H
#define CPP_NETWORK_EVENTFD_H

#include "file_descriptor.h"

#include <cstdint>

namespace network
{
	class event_descriptor : public base_descriptor_resource
	{
	public:
		event_descriptor();

		event_descriptor(event_descriptor &&rhs) noexcept;

		uint64_t read();

		void write(uint64_t value);
	};
}

#endif //CPP_NETWORK_EVENTFD_H
