#include "network/epoll.h"
#include "network/socket.h"

#include <memory>
#include <map>
#include <iostream>

class connection
{
public:
	network::client_socket client_;
	network::epoll_registration client_registration_;

	connection(network::client_socket &&client,
	           network::epoll &epoll,
	           std::map<int, std::unique_ptr<connection>> &map) :
			client_{std::move(client)}, client_registration_{&this->client_.get_fd(), &epoll}
	{
		client_registration_.set_on_read([this] {
			std::cout << this->client_.read();
		});

		client_registration_.set_cleanup([this, &map] {
			map.erase(this->client_.get_fd().get_raw_fd());
		});
	}
};

int main()
{
	network::server_socket server{network::make_local_endpoint(2539)};
	network::epoll epoll{};
	network::epoll_registration server_registration{&server.get_fd(), &epoll};
	std::map<int, std::unique_ptr<connection>> map;

	server_registration.set_on_read([&] {
		std::unique_ptr<connection> conn = std::make_unique<connection>(server.accept(), epoll, map);
		connection *raw_conn = conn.get();
		epoll.add(conn->client_registration_);
		map.insert(std::make_pair(raw_conn->client_.get_fd().get_raw_fd(), std::move(conn)));
	});

	server_registration.set_cleanup([&] {
		epoll.soft_stop();
	});

	epoll.add(server_registration);
	epoll.run();

	return 0;
}