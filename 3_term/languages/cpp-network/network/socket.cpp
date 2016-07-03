#include "socket.h"
#include "network_exception.h"

#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

namespace network
{


	client_socket::client_socket(file_descriptor &&fd) : fd{std::move(fd)}
	{
	}


	file_descriptor const &client_socket::get_fd()
	{
		return fd;
	}


	std::string client_socket::read()
	{
		char buf[1024];
		ssize_t read_n = ::read(fd.get_raw_fd(), buf, sizeof buf);
		check_return_code(read_n);
		//return std::string{buf, (size_t) read_n};
		return std::string(buf, (size_t) read_n);
	}


	size_t client_socket::write(std::string str)
	{
		ssize_t written = ::write(fd.get_raw_fd(), str.c_str(), str.size());
		check_return_code(written);
		return static_cast<size_t>(written);
	}


	socket::socket(file_descriptor &&fd) : fd(std::move(fd))
	{
	}


	socket::socket(uint16_t port) : socket(file_descriptor(::socket(AF_INET, SOCK_STREAM, 0)))
	{
		int enable = 1;
		setsockopt(fd.get_raw_fd(), SOL_SOCKET, SO_REUSEPORT, &enable, sizeof(enable));

		struct sockaddr_in address;
		address.sin_family = AF_INET;
		address.sin_addr.s_addr = INADDR_ANY;
		address.sin_port = htons(port);

		check_return_code(
				bind(fd.get_raw_fd(), static_cast<sockaddr*>(&address), sizeof address));

		check_return_code(
				listen(fd.get_raw_fd(), SOMAXCONN));
	}


	client_socket socket::accept()
	{
		int new_fd = ::accept(fd.get_raw_fd(), nullptr, nullptr);
		return network::client_socket(file_descriptor(new_fd));
	}


}