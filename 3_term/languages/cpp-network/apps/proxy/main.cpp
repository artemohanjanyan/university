#include <network/socket.h>
#include <network/epoll.h>
#include <utils/string_buffer.h>

#include <memory>
#include <map>
#include <iostream>
#include <utils/log.h>

struct connection
{
	struct host_data
	{
		network::client_socket host;
		network::epoll_registration host_registration;

		host_data(network::client_socket &&host, network::epoll &epoll) :
				host{std::move(host)}, host_registration{this->host.get_fd(), epoll}
		{
		}
	};

	network::client_socket client;
	network::epoll_registration client_registration;
	std::string request;

	std::unique_ptr<host_data> host;
	utils::string_buffer write_buffer;
	utils::string_buffer read_buffer;

	network::epoll &epoll;
	std::map<int, std::unique_ptr<connection>> &map;

	network::callback host_write;

	void setup_exchange()
	{
		size_t pos1 = request.find("ost: ");
		size_t pos2 = request.find("\r\n", pos1);
		std::string host_name = request.substr(pos1 + 5, pos2 - pos1 - 5);

		host = std::make_unique<host_data>(network::client_socket{network::get_hosts(host_name)}, epoll);
		write_buffer.push(request);

		host_write = [this] {
			write_buffer.pop(host->host.write(write_buffer.top()));
			if (write_buffer.is_empty())
				host->host_registration.unset_on_write().update();
		};

		client_registration.set_on_read([this] {
			if (write_buffer.is_empty())
				host->host_registration.set_on_write(host_write).update();
			write_buffer.push(host->host.read());
		}).update();

		host->host_registration.set_on_read([this] {
			if (read_buffer.is_empty())
				client_registration.set_on_write([this] {
					read_buffer.pop(client.write(read_buffer.top()));
					if (read_buffer.is_empty())
						client_registration.unset_on_write().update();
				}).update();
			read_buffer.push(client.read());
		});

		host->host_registration.set_on_write(host_write);

		host->host_registration.set_on_close([this] {
			epoll.schedule_cleanup(client_registration);
		});

		host->host_registration.set_cleanup([this] {
			map.erase(client.get_fd().get_raw_fd());
		});

		epoll.add(host->host_registration);
	}

	connection(network::client_socket &&client_init,
	           network::epoll &epoll_init,
	           std::map<int, std::unique_ptr<connection>> &map_init) :
			client{std::move(client_init)}, client_registration{this->client.get_fd(), epoll_init},
			epoll{epoll_init}, map{map_init}
	{
		client_registration.set_on_read([this] {
			request += client.read();
			if (request.find("\r\n\r\n") != std::string::npos)
				setup_exchange();
		});

		client_registration.set_cleanup([this] {
			map.erase(client.get_fd().get_raw_fd());
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