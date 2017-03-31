#ifndef CPP_NETWORK_RESOLVER_H
#define CPP_NETWORK_RESOLVER_H

#include "utils/executor.h"
#include "network/socket.h"
#include "network/event_descriptor.h"

namespace network { namespace http
{
	class resolver
	{
		utils::executor executor_;
		utils::blocking_queue<std::pair<std::string, std::vector<network::ipv4_endpoint>>> result_queue_;
		network::event_descriptor *event_descriptor_;

	public:
		resolver(size_t thread_n,
		         network::event_descriptor *event_descriptor);

		void enqueue_host(std::string const &host);

		std::pair<std::string, std::vector<network::ipv4_endpoint>> dequeue_result();
	};
}}

#endif //CPP_NETWORK_RESOLVER_H
