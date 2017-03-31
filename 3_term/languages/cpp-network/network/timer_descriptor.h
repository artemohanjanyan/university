#ifndef CPP_NETWORK_TIMER_DESCRIPTOR_H
#define CPP_NETWORK_TIMER_DESCRIPTOR_H

#include "file_descriptor.h"

#include <cstdint>

#include <chrono>

namespace network
{
	class timer_descriptor : public base_descriptor_resource
	{
	public:
		timer_descriptor();

		timer_descriptor(timer_descriptor &&rhs) noexcept;

		uint64_t get_count();

		void set_time(std::chrono::seconds const &duration);
	};
}

#endif //CPP_NETWORK_TIMER_DESCRIPTOR_H
