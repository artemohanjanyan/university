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
		client_registration_
				.set_on_read([this] {
					std::cout << this->client_.read();
				})
				.set_cleanup([this, &map] {
					map.erase(this->client_.get_fd().get_raw_fd());
				})
				.update();
	}
};

int main()
{
	network::server_socket server{network::make_local_endpoint(2539)};
	network::epoll epoll{};
	network::epoll_registration server_registration{&server.get_fd(), &epoll};
	std::map<int, std::unique_ptr<connection>> map;

	server_registration
			.set_on_read([&] {
				std::unique_ptr<connection> conn = std::make_unique<connection>(server.accept(), epoll, map);
				map.insert(std::make_pair(conn.get()->client_.get_fd().get_raw_fd(), std::move(conn)));
			})
			.set_cleanup([&] {
				epoll.soft_stop();
			})
			.update();

	epoll.run();

	return 0;
}