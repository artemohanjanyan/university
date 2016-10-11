#include "network/socket.h"

int main()
{
	network::server_socket server{network::make_local_endpoint(2539)};
	network::client_socket client{server.accept()};

	std::string str;
	do
	{
		str = client.read();
		client.write(str);
	} while (!str.empty());

	return 0;
}
