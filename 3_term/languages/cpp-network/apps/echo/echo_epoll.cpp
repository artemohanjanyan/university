#include "network/socket.h"
#include "network/epoll.h"
#include "network/network_exception.h"
#include "utils/string_buffer.h"

#include <map>
#include <memory>
#include <iostream>
#include <utils/log.h>

class connection
{
public:
	network::client_socket client;
	network::epoll_registration client_registration;
	utils::string_buffer string_buffer{};

	connection(network::client_socket &&client_init,
	           network::epoll &epoll, std::map<int, std::unique_ptr<connection>> &map) :
			client{std::move(client_init)}, client_registration{this->client.get_fd(), epoll}
	{
		client_registration.set_on_read([this] {
			std::string msg = client.read();
			if (string_buffer.is_empty())
				client_registration.set_on_write([this] {
					size_t written = client.write(string_buffer.top());
					string_buffer.pop(written);
					if (string_buffer.is_empty())
						client_registration.set_on_write(nullptr).update();
				}).update();
			string_buffer.push(msg);
		});

		client_registration.set_cleanup([this, &map] {
			map.erase(this->client.get_fd().get_raw_fd());
		});
	}
};

int main()
{
	network::log.print_mask |= utils::verbose;

	network::server_socket server{network::make_local_endpoint(2539)};
	network::epoll epoll{};
	network::epoll_registration server_registration{server.get_fd(), epoll};
	std::map<int, std::unique_ptr<connection>> map;

	server_registration.set_on_read([&] {
		std::unique_ptr<connection> unique_conn = std::make_unique<connection>(server.accept(), epoll, map);
		connection *conn = unique_conn.get();
		epoll.add(conn->client_registration);
		map.insert(std::make_pair(unique_conn->client.get_fd().get_raw_fd(), std::move(unique_conn)));
	});

	server_registration.set_cleanup([&] {
		epoll.soft_stop();
	});

	epoll.add(server_registration);
	std::cout << to_string(get_socket_endpoint(server.get_fd())) << "\n";
	epoll.run();

	return 0;
}
