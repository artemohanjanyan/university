#include "network/epoll.h"
#include "network/socket.h"

#include <memory>
#include <map>
#include <iostream>

class connection
{
public:
	network::client_socket client;
	network::epoll_registration client_registration;

	connection(network::client_socket &&client,
	           network::epoll &epoll,
	           std::map<int, std::unique_ptr<connection>> &map) :
			client{std::move(client)}, client_registration{this->client.get_fd(), epoll}
	{
		client_registration.set_on_read([this] {
			// Not actually correct, but nobody cares
			std::string string = this->client.read();
			std::cout << int(string[string.size() - 2]) << " " << int(string[string.size() - 1]) << "\n";
			string = string.substr(0, string.size() - 2);
			for (auto endpoint : network::get_hosts(string))
				this->client.write(to_string(endpoint) + "\n");
		});

		client_registration.set_cleanup([this, &map] {
			map.erase(this->client.get_fd().get_raw_fd());
		});
	}
};

int main()
{
	network::server_socket server{network::make_local_endpoint(2539)};
	network::epoll epoll{};
	network::epoll_registration server_registration{server.get_fd(), epoll};
	std::map<int, std::unique_ptr<connection>> map;

	server_registration.set_on_read([&] {
		std::unique_ptr<connection> conn = std::make_unique<connection>(server.accept(), epoll, map);
		connection *raw_conn = conn.get();
		epoll.add(conn->client_registration);
		map.insert(std::make_pair(raw_conn->client.get_fd().get_raw_fd(), std::move(conn)));
	});

	server_registration.set_cleanup([&] {
		epoll.soft_stop();
	});

	epoll.add(server_registration);
	epoll.run();

	return 0;
}