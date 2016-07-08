#include "network/socket.h"
#include "network/epoll.h"
#include "network/utils/string_buffer.h"

#include <map>
#include <memory>

class connection
{
public:
	network::client_socket client;
	network::epoll_registration registration;
	network::utils::string_buffer string_buffer{};

	connection(network::client_socket &&client) :
			client{std::move(client)}, registration{this->client.get_fd()}
	{
	}
};

int main()
{
	network::server_socket server{2539};
	network::epoll epoll{};
	network::epoll_registration server_registration{server.get_fd()};
	std::map<int, std::unique_ptr<connection>> map;

	server_registration.set_on_read([&] {
		std::unique_ptr<connection> unique_conn = std::make_unique<connection>(server.accept());
		int raw_fd = unique_conn->client.get_fd().get_raw_fd();
		connection *conn = unique_conn.get();

		conn->registration.set_on_read([conn, &epoll] {
			std::string msg = conn->client.read();
			if (conn->string_buffer.is_empty())
			{
				conn->registration.set_on_write([conn, &epoll] {
					size_t written = conn->client.write(conn->string_buffer.top());
					conn->string_buffer.pop(written);
					if (conn->string_buffer.is_empty())
					{
						conn->registration.unset_on_write();
						epoll.update(conn->registration);
					}
				});
				epoll.update(conn->registration);
			}
			conn->string_buffer.push(msg);
		});

		conn->registration.set_on_close([raw_fd, conn, &epoll, &map] {
			auto it = map.find(raw_fd);
			epoll.remove(conn->registration);
			map.erase(it);
		});

		epoll.add(conn->registration);
		map.insert(std::make_pair(raw_fd, std::move(unique_conn)));
	});

	epoll.add(server_registration);
	epoll.run();

	return 0;
}
