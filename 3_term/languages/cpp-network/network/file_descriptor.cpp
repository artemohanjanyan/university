#include "file_descriptor.h"
#include "network_exception.h"

#include <sys/socket.h>
#include <unistd.h>

#include <iostream>

namespace network
{
	file_descriptor::file_descriptor(int fd) noexcept : fd(fd)
	{
#ifdef CPP_NETWORK_DEBUG
		std::cerr << "file_descriptor(" << fd << ")\n";
#endif
	}

	file_descriptor::file_descriptor(file_descriptor &&rhs) noexcept : fd(rhs.fd)
	{
		rhs.fd = -1;
	}

	file_descriptor::~file_descriptor() noexcept
	{
		if (fd == -1)
			return;

#ifdef CPP_NETWORK_DEBUG
		std::cerr << "~file_descriptor(" << fd << "), ";
		int error = 0;
		socklen_t err_len = sizeof error;
		if (getsockopt(fd, SOL_SOCKET, SO_ERROR, static_cast<void *>(&error), &err_len) == 0)
			std::cerr << "last error: " << strerror(error);
		std::cerr << std::endl;
#endif

		close(fd);
	}

	file_descriptor &file_descriptor::operator=(file_descriptor &&rhs) noexcept
	{
		std::swap(fd, rhs.fd);
		return *this;
	}

	int file_descriptor::get_raw_fd() const noexcept
	{
		return fd;
	}

	bool file_descriptor::is_set() const noexcept
	{
		return fd != -1;
	}

	base_descriptor_resource::base_descriptor_resource(network::file_descriptor &&fd) noexcept : fd(std::move(fd))
	{
	}

	file_descriptor const &base_descriptor_resource::get_fd() const noexcept
	{
		return fd;
	}
}