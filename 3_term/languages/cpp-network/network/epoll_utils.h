#ifndef CPP_NETWORK_EPOLL_UTILS_H
#define CPP_NETWORK_EPOLL_UTILS_H

#include "socket.h"
#include "epoll.h"
#include "utils/string_buffer.h"

namespace network
{
	void do_on_connect(client_socket *client, epoll_registration *registration, callback on_connect);

	void forward_buffer(utils::string_buffer *buffer,
	                    network::client_socket *dst,
	                    network::epoll_registration *dst_registration);

	void forward_buffer_if_empty(utils::string_buffer *buffer,
	                             network::client_socket *dst,
	                             network::epoll_registration *dst_registration);
}

#endif //CPP_NETWORK_EPOLL_UTILS_H
