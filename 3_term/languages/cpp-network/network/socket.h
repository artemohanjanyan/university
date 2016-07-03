#ifndef CPP_NETWORK_SERVER_SOCKET_H
#define CPP_NETWORK_SERVER_SOCKET_H

#include "file_descriptor.h"

#include <stdint.h>

#include <string>

namespace network
{


	class client_socket
	{
		file_descriptor fd;

	public:
		client_socket(file_descriptor &&fd);

		file_descriptor const& get_fd();

		std::string read();
		size_t write(std::string);
	};


	class socket
	{
		file_descriptor fd;

	public:
		socket(file_descriptor &&fd);
		socket(uint16_t port);

		client_socket accept();
	};


}


#endif //CPP_NETWORK_SERVER_SOCKET_H
