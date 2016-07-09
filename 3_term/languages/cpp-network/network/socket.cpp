#include "socket.h"
#include "network_exception.h"

#include <sys/socket.h>
#include <netinet/in.h>

namespace network
{
	client_socket::client_socket(file_descriptor &&fd) noexcept : base_descriptor_resource{std::move(fd)}
	{
	}

	client_socket::client_socket(client_socket &&rhs) noexcept : client_socket{std::move(rhs.fd)}
	{
	}

	std::string client_socket::read()
	{
		char buf[1024];
		ssize_t read_n = ::recv(fd.get_raw_fd(), buf, sizeof buf, MSG_NOSIGNAL);
		check_return_code(read_n);
		return std::string(buf, static_cast<size_t>(read_n));
	}

	size_t client_socket::write(utils::string_view const &str)
	{
		ssize_t written = ::send(fd.get_raw_fd(), str.begin(), str.size(), MSG_NOSIGNAL);
		check_return_code(written);
		return static_cast<size_t>(written);
	}

	server_socket::server_socket(file_descriptor &&fd) : base_descriptor_resource{std::move(fd)}
	{
	}

	server_socket::server_socket(server_socket &&rhs) : server_socket{std::move(rhs.fd)}
	{
	}

	server_socket::server_socket(uint16_t port) :
			server_socket{file_descriptor{check_return_code(::socket(AF_INET, SOCK_STREAM, 0))}}
	{
		int enable = 1;
		setsockopt(fd.get_raw_fd(), SOL_SOCKET, SO_REUSEPORT, &enable, sizeof enable);

		struct sockaddr_in address;
		address.sin_family = AF_INET;
		address.sin_addr.s_addr = INADDR_ANY;
		address.sin_port = htons(port);

		check_return_code(
				bind(fd.get_raw_fd(), reinterpret_cast<sockaddr *>(&address), sizeof address));

		check_return_code(
				listen(fd.get_raw_fd(), SOMAXCONN));
	}

	client_socket server_socket::accept()
	{
		int new_fd = ::accept(fd.get_raw_fd(), nullptr, nullptr);
		check_return_code(new_fd);
		return network::client_socket{file_descriptor{new_fd}};
	}
}