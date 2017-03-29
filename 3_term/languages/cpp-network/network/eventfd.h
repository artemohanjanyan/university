#ifndef CPP_NETWORK_EVENTFD_H
#define CPP_NETWORK_EVENTFD_H

#include "file_descriptor.h"

#include <cstdint>

namespace network
{
	class eventfd : public base_descriptor_resource
	{
	public:
		eventfd();

		eventfd(eventfd &&rhs) noexcept;

		uint64_t read();

		void write(uint64_t value);
	};
}

#endif //CPP_NETWORK_EVENTFD_H
