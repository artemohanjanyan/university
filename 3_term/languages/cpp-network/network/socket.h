#ifndef CPP_NETWORK_SERVER_SOCKET_H
#define CPP_NETWORK_SERVER_SOCKET_H

#include "file_descriptor.h"

#include <stdint.h>

#include <string>

namespace network
{
	class client_socket : public base_descriptor_resource
	{
	public:
		client_socket(file_descriptor &&fd) noexcept;

		client_socket(client_socket &&rhs) noexcept;

		std::string read();

		size_t write(std::string);
	};

	class server_socket : public base_descriptor_resource
	{
	public:
		server_socket(file_descriptor &&fd);

		server_socket(server_socket &&rhs);

		server_socket(uint16_t port);

		client_socket accept();
	};
}


#endif //CPP_NETWORK_SERVER_SOCKET_H
