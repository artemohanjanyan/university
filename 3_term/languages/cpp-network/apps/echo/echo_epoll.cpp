#include "network/socket.h"
#include "network/epoll.h"
#include "network/file_descriptor.h"
#include "network/network_exception.h"
#include "utils/string_buffer.h"

#include <map>
#include <memory>
#include <iostream>

class connection
{
public:
	network::client_socket client;
	network::epoll_registration registration;
	utils::string_buffer string_buffer{};

	connection(network::client_socket &&client,
	           network::epoll &epoll, std::map<int, std::unique_ptr<connection>> &map) :
			client{std::move(client)}, registration{this->client.get_fd()}
	{
		registration.set_on_read([this, &epoll] {
			std::string msg = this->client.read();
			if (this->string_buffer.is_empty())
			{
				this->registration.set_on_write([this, &epoll] {
					size_t written = this->client.write(this->string_buffer.top());
					this->string_buffer.pop(written);
					if (this->string_buffer.is_empty())
					{
						this->registration.unset_on_write();
						epoll.update(this->registration);
					}
				});
				epoll.update(this->registration);
			}
			this->string_buffer.push(msg);
		});

		registration.set_on_close([this, &epoll] {
			epoll.schedule_cleanup(this->registration);
		});

		registration.set_cleanup([this, &epoll, &map] {
			auto it = map.find(this->client.get_fd().get_raw_fd());
			epoll.remove(this->registration);
			map.erase(it);
		});
	}
};

int main()
{
	network::server_socket server{network::make_local_endpoint(2539)};
	network::epoll epoll{};
	network::epoll_registration server_registration{server.get_fd()};
	std::map<int, std::unique_ptr<connection>> map;

	server_registration.set_on_read([&] {
		std::unique_ptr<connection> unique_conn = std::make_unique<connection>(server.accept(), epoll, map);
		connection *conn = unique_conn.get();

		epoll.add(conn->registration);
		map.insert(std::make_pair(unique_conn->client.get_fd().get_raw_fd(), std::move(unique_conn)));
	});

	epoll.add(server_registration);
	std::cout << to_string(get_socket_endpoint(server.get_fd())) << "\n";
	epoll.run();

	return 0;
}
