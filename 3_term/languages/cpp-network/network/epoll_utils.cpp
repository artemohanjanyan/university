#include "epoll_utils.h"

namespace network
{
	void do_on_connect(client_socket *client, epoll_registration *registration, callback on_connect)
	{
		registration->set_on_write([client, registration, on_connect{std::move(on_connect)}] {
			client->assert_availability();
			on_connect();
		}).update();
	}

	void forward_buffer(utils::string_buffer *buffer,
	                    network::client_socket *dst,
	                    network::epoll_registration *dst_registration)
	{
		dst_registration->set_on_write([buffer, dst, dst_registration] {
			buffer->pop(dst->write(buffer->top()));
			if (buffer->is_empty())
				dst_registration->set_on_write(nullptr).update();
		}).update();
	}

	void forward_buffer_if_empty(utils::string_buffer *buffer,
	                             network::client_socket *dst,
	                             network::epoll_registration *dst_registration)
	{
		if (buffer->is_empty())
			forward_buffer(buffer, dst, dst_registration);
	}
}
