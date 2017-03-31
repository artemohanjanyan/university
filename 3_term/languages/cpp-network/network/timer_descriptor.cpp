#include "timer_descriptor.h"
#include "network_exception.h"
#include "utils/log.h"

#include <unistd.h>
#include <sys/timerfd.h>

namespace network
{
	timer_descriptor::timer_descriptor()
			: base_descriptor_resource{file_descriptor{check_return_code(timerfd_create(CLOCK_MONOTONIC, 0))}}
	{}

	timer_descriptor::timer_descriptor(timer_descriptor &&rhs) noexcept
			: base_descriptor_resource{std::move(rhs.fd)}
	{}

	uint64_t timer_descriptor::get_count()
	{
		log(utils::info) << "get count from " << std::to_string(fd.get_raw_fd()) << "\n";
		uint64_t expiration_n;
		ssize_t read_n = check_return_code(::read(fd.get_raw_fd(), &expiration_n, sizeof(uint64_t)));
		if (read_n != sizeof(uint64_t))
			throw network_exception{"incomplete read from timer"};
		return expiration_n;
	}

	void timer_descriptor::set_time(std::chrono::seconds const &duration)
	{
		log(utils::info) << "set time on " << std::to_string(fd.get_raw_fd()) << "\n";
		itimerspec new_value = {}, old_value = {};
		new_value.it_value.tv_sec = duration.count();
		check_return_code(timerfd_settime(fd.get_raw_fd(), 0, &new_value, &old_value));
	}
}