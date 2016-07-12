#include "network/epoll.h"
#include "network/socket.h"

#include <memory>
#include <map>
#include <iostream>

class connection
{
public:
	network::client_socket client;
	network::epoll_registration registration;

	connection(network::client_socket &&client,
	           network::epoll &epoll,
	           std::map<int, std::unique_ptr<connection>> &map) :
			client{std::move(client)}, registration{this->client.get_fd()}
	{
		registration.set_on_read([this, &epoll] {
			std::cout << this->client.read();
		});

		registration.set_on_close([this, &epoll] {
			epoll.schedule_cleanup(this->registration);
		});

		registration.set_cleanup([this, &epoll, &map] {
			epoll.remove(this->registration);
			map.erase(this->client.get_fd().get_raw_fd());
		});
	}
};

int main()
{
	network::server_socket server{network::make_local_endpoint(2539)};
	network::epoll_registration server_registration{server.get_fd()};
	network::epoll epoll{};
	std::map<int, std::unique_ptr<connection>> map;

	server_registration.set_on_read([&] {
		std::unique_ptr<connection> conn = std::make_unique<connection>(server.accept(), epoll, map);
		connection *raw_conn = conn.get();
		epoll.add(conn->registration);
		map.insert(std::make_pair(raw_conn->client.get_fd().get_raw_fd(), std::move(conn)));
	});

	server_registration.set_cleanup([&] {
		epoll.soft_stop();
	});

	epoll.add(server_registration);
	epoll.run();

	return 0;
}