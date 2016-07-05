#include <network/socket.h>
#include <network/epoll.h>

#include <map>
#include <memory>

int main()
{
	network::server_socket server{2539};
	network::epoll epoll{};
	network::epoll_registration server_registration{};

	using info_type = std::pair<network::client_socket, network::epoll_registration>;
	std::map<int, std::unique_ptr<info_type>> map;

	server_registration.set_on_read([&] {
		network::client_socket client = server.accept();
		network::epoll_registration registration{};

		int raw_fd = client.get_fd().get_raw_fd();
		std::unique_ptr<info_type> ptr;
		ptr = std::make_unique<info_type>(std::move(client), std::move(registration));

		ptr->second.set_on_read([raw_ptr = ptr.get()] {
			std::string msg = raw_ptr->first.read();
			raw_ptr->first.write(msg);
		});
		ptr->second.set_on_close([raw_fd, &map, &epoll] {
			auto it = map.find(raw_fd);
			epoll.del(it->second->first.get_fd());
			map.erase(it);
		});

		epoll.add(ptr->first.get_fd(), ptr->second);
		map.insert(std::make_pair(raw_fd, std::move(ptr)));
	});

	epoll.add(server.get_fd(), server_registration);
	epoll.run();

	return 0;
}
