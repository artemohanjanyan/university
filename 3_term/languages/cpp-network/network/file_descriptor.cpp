#include "file_descriptor.h"
#include "network_exception.h"
#include "utils/log.h"

#include <sys/socket.h>
#include <unistd.h>
#include <fcntl.h>

#include <iostream>

namespace network
{
	utils::log log{std::cout};

	file_descriptor::file_descriptor() noexcept
	{}

	file_descriptor make_blocking_fd(int fd) noexcept
	{
		file_descriptor descriptor{};
		descriptor.fd = fd;
		return std::move(descriptor);
	}

	file_descriptor::file_descriptor(int fd)
			: fd(fd)
	{
		log(utils::info) << "file_descriptor(" << *this << ")\n";
		int flags;
		check_return_code(
				flags = fcntl(fd, F_GETFL, 0));
		check_return_code(
				fcntl(fd, F_SETFL, flags | O_NONBLOCK));
	}

	file_descriptor::file_descriptor(file_descriptor &&rhs) noexcept
			: fd(rhs.fd)
	{
		rhs.fd = -1;
	}

	file_descriptor::~file_descriptor() noexcept
	{
		if (fd == -1)
			return;

		log(utils::info) << "~file_descriptor(" << *this << "), ";
		int error = 0;
		socklen_t err_len = sizeof error;
		if (getsockopt(fd, SOL_SOCKET, SO_ERROR, static_cast<void *>(&error), &err_len) == 0)
			log(utils::info) << "last error: " << strerror(error);
		log(utils::info) << "\n";

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

	base_descriptor_resource::base_descriptor_resource(network::file_descriptor &&fd) noexcept
			: fd(std::move(fd))
	{}

	file_descriptor const &base_descriptor_resource::get_fd() const noexcept
	{
		return fd;
	}
}