#include "network/socket.h"

int main()
{
	network::socket server{2539};
	network::client_socket client{server.accept()};

	std::string str;
	do
	{
		str = client.read();
		client.write(str);
	} while (!str.empty());

	return 0;
}