#ifndef CPP_NETWORK_SERVER_SOCKET_H
#define CPP_NETWORK_SERVER_SOCKET_H

#include "file_descriptor.h"
#include "utils/string_view.h"

#include <stdint.h>

#include <string>

namespace network
{
	class ipv4_address
	{
		uint32_t address;

	public:
		ipv4_address(uint32_t address) noexcept;

		uint32_t get_raw_address() const noexcept;
	};

	ipv4_address make_address_any() noexcept;

	std::string to_string(ipv4_address address);


	class ipv4_endpoint
	{
		ipv4_address address;
		uint16_t port_n;

		ipv4_endpoint(ipv4_address address, uint16_t port_n) noexcept;

		friend ipv4_endpoint make_ipv4_endpoint_h(ipv4_address address, uint16_t port_h) noexcept;

		friend ipv4_endpoint make_ipv4_endpoint_n(ipv4_address address, uint16_t port_n) noexcept;

		friend ipv4_endpoint make_any_endpoint(ipv4_address address) noexcept;

		friend ipv4_endpoint make_local_endpoint(uint16_t port_h) noexcept;

	public:
		ipv4_address get_address() const noexcept;

		uint16_t get_port_n() const noexcept;

		uint16_t get_port_h() const noexcept;
	};

	ipv4_endpoint make_ipv4_endpoint_h(ipv4_address address, uint16_t port_h) noexcept;

	ipv4_endpoint make_ipv4_endpoint_n(ipv4_address address, uint16_t port_n) noexcept;

	ipv4_endpoint make_any_endpoint(ipv4_address address = make_address_any()) noexcept;

	ipv4_endpoint make_local_endpoint(uint16_t port_h) noexcept;

	std::string to_string(ipv4_endpoint endpoint);


	class client_socket : public base_descriptor_resource
	{
	public:
		client_socket(file_descriptor &&fd) noexcept;

		client_socket(client_socket &&rhs) noexcept;

		std::string read();

		size_t write(utils::string_view const &str);
	};


	class server_socket : public base_descriptor_resource
	{
	public:
		server_socket(file_descriptor &&fd);

		server_socket(server_socket &&rhs);

		server_socket(ipv4_endpoint endpoint);

		client_socket accept();
	};


	ipv4_endpoint get_socket_endpoint(file_descriptor const &fd);
}


#endif //CPP_NETWORK_SERVER_SOCKET_H
