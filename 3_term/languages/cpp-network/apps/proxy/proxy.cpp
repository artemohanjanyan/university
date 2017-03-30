#include "network/socket.h"
#include "network/epoll.h"
#include "network/epoll_utils.h"
#include "network/http/request.h"
#include "network/http/request_parser.h"
#include "utils/log.h"
#include "network/event_descriptor.h"

#include <map>
#include <iostream>

utils::log log{std::cout};

struct connection;

struct proxy_server
{
	network::server_socket server_;
	network::epoll epoll_;
	network::epoll_registration server_registration_;
	std::map<int, std::unique_ptr<connection>> map_;

	std::unique_ptr<network::event_descriptor> event_descriptor_;
	network::epoll_registration event_registration_;
	std::map<std::string, connection *> host_to_conn_;

	proxy_server(network::ipv4_endpoint endpoint);

	void run();
};

struct host_data
{
	network::client_socket host_;
	network::epoll_registration host_registration_;
	std::string host_name_;

	host_data(network::client_socket &&host, std::string host_name, connection *conn);
};

struct connection
{
	proxy_server *server_;

	network::client_socket client_;
	network::epoll_registration client_registration_;
	network::http::request_parser parser_;
	network::http::request_parser_registration parser_registration_;

	std::unique_ptr<host_data> host_;
	utils::string_buffer write_buffer_;
	utils::string_buffer read_buffer_;

	void connect_to_host(std::string new_host_name)
	{
		auto connect_task = [this, new_host_name{std::move(new_host_name)}] {
			std::vector<network::ipv4_endpoint> hosts = network::get_hosts(new_host_name);
			host_ = std::make_unique<host_data>(
					network::client_socket{network::connect(hosts, true)},
					std::move(new_host_name),
					this);
		};

		if (host_ == nullptr)
			connect_task();
		else
		{
			host_->host_registration_.set_cleanup(connect_task);
			server_->epoll_.schedule_cleanup(host_->host_registration_);
		}
	}

	connection(proxy_server *server,
	           network::client_socket &&client_init)
			: server_{server}
			, client_{std::move(client_init)}
			, client_registration_{&this->client_.get_fd(), &server_->epoll_}
			, parser_{}
			, parser_registration_{}
	{
		parser_.register_consumer(&parser_registration_);

		parser_registration_
				.set_request_consumer([this] (network::http::request request) {
					std::string new_host_name = request.headers().at("Host");
					if (host_ == nullptr || host_->host_name_ != new_host_name)
					{
						write_buffer_ = {};
						read_buffer_ = {};
						connect_to_host(std::move(new_host_name));
					}
					else
						forward_buffer_if_empty(&write_buffer_, &host_->host_, &host_->host_registration_);
					log(utils::verbose) << to_string(request);
					write_buffer_.push(to_string(request, host_->host_name_));
				})
				.set_chunk_consumer([this] (std::string chunk) {
					log(utils::verbose) << chunk;
					write_buffer_.push(std::move(chunk));
				});

		client_registration_
				.set_on_read([this] {
					parser_.parse(client_.read());
				})
				.set_cleanup([this] {
					server_->map_.erase(client_.get_fd().get_raw_fd());
				})
				.update();
	}
};

host_data::host_data(network::client_socket &&host, std::string host_name, connection *conn)
		: host_{std::move(host)}
		, host_registration_{&host_.get_fd(), &conn->server_->epoll_}
		, host_name_{std::move(host_name)}
{
	host_registration_
			.set_on_read([this, conn] {
				forward_buffer_if_empty(&conn->read_buffer_, &conn->client_, &conn->client_registration_);
				conn->read_buffer_.push(host_.read());
			})
			.set_on_close([this, conn] {
				conn->server_->epoll_.schedule_cleanup(conn->client_registration_);
			})
			.set_cleanup([this, conn] {
				log(utils::error) << "Strange host cleanup\n";
				conn->server_->map_.erase(conn->client_.get_fd().get_raw_fd());
			})
			.update();

	network::do_on_connect(&host_, &host_registration_, [this, conn] {
		if (conn->write_buffer_.is_empty())
			host_registration_.set_on_write(nullptr).update();
		else
			forward_buffer(&conn->write_buffer_, &host_, &host_registration_);
	});
}

proxy_server::proxy_server(network::ipv4_endpoint endpoint)
		: server_{endpoint}
		, epoll_{}
		, server_registration_{&server_.get_fd(), &epoll_}
		, map_{}

		, event_descriptor_{std::make_unique<network::event_descriptor>()}
		, event_registration_{&event_descriptor_->get_fd(), &epoll_}
		, host_to_conn_{}
{
	make_non_blocking(server_.get_fd());
	make_non_blocking(event_descriptor_->get_fd());

	server_registration_
			.set_on_read([this] {
				std::unique_ptr<connection> conn = std::make_unique<connection>(this, server_.accept());
				connection *raw_conn = conn.get();
				map_.insert(std::make_pair(raw_conn->client_.get_fd().get_raw_fd(), std::move(conn)));
			})
			.set_cleanup([this] {
				epoll_.soft_stop();
			})
			.update();

	event_registration_
			.set_on_read([this] {
				// TODO
			})
			.update();
}

void proxy_server::run()
{
	epoll_.run();
}

int main()
{
	network::log.print_mask |= utils::verbose;
	log.print_mask |= utils::verbose;
	log(utils::debug) << "Debug works\n";

	proxy_server server{network::make_local_endpoint(2539)};
	server.run();

	return 0;
}