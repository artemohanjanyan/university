#include "resolver.h"

namespace network { namespace http
{

	resolver::resolver(size_t thread_n, network::event_descriptor *event_descriptor)
			: executor_{thread_n}
			, event_descriptor_{event_descriptor}
	{}

	void resolver::enqueue_host(std::string const &host)
	{
		executor_.push_task([this, host] {
			result_queue_.push({host, network::get_hosts(host)});
			event_descriptor_->write(1);
		});
	}

	std::pair<std::string, std::vector<network::ipv4_endpoint>> resolver::dequeue_result()
	{
		return result_queue_.pop();
	}
}}