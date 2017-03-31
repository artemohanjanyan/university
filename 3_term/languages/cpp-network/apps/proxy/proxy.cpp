#include "network/socket.h"
#include "network/epoll.h"
#include "network/epoll_utils.h"
#include "network/http/request.h"
#include "network/http/request_parser.h"
#include "network/http/resolver.h"
#include "utils/log.h"

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
	std::map<std::string, std::set<connection *>> host_to_conn_;
	network::http::resolver resolver_;

	proxy_server(network::ipv4_endpoint endpoint, size_t thread_n);

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

	std::string waiting_host_name;

	void enqueue_resolving(std::string new_host_name)
	{
		log(utils::info) << "enqueue resolving of " << new_host_name << "\n";

		if (waiting_host_name != "")
			server_->host_to_conn_.at(waiting_host_name).erase(this);
		waiting_host_name = new_host_name;

		write_buffer_ = {};
		read_buffer_ = {};

		if (!server_->host_to_conn_.count(new_host_name))
		{
			log(utils::info) << "\tadd " << new_host_name << " to resolver\n";
			server_->resolver_.enqueue_host(new_host_name);
		}
		server_->host_to_conn_[new_host_name].insert(this);
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
						enqueue_resolving(new_host_name);
					else
						forward_buffer_if_empty(&write_buffer_, &host_->host_, &host_->host_registration_);
					log(utils::verbose) << to_string(request);
					write_buffer_.push(to_string(request, new_host_name));
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

	~connection()
	{
		if (waiting_host_name != "")
			server_->host_to_conn_.at(waiting_host_name).erase(this);
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
				log(utils::error) << "strange host cleanup\n";
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

proxy_server::proxy_server(network::ipv4_endpoint endpoint, size_t thread_n)
		: server_{endpoint}
		, epoll_{}
		, server_registration_{&server_.get_fd(), &epoll_}
		, map_{}

		, event_descriptor_{std::make_unique<network::event_descriptor>()}
		, event_registration_{&event_descriptor_->get_fd(), &epoll_}
		, host_to_conn_{}
		, resolver_{thread_n, event_descriptor_.get()}
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
				size_t resolved_n = event_descriptor_->read();
				log(utils::info) << "read " << std::to_string(resolved_n) << " resolved addresses\n";
				for (size_t i = 0; i < resolved_n; ++i)
				{
					auto result = resolver_.dequeue_result();
					log(utils::info) << "\tresolved address for " << result.first << "\n";
					for (connection *conn : host_to_conn_.at(result.first))
					{
						conn->waiting_host_name = "";

						auto connect_task = [result, conn] {
							conn->host_ = std::make_unique<host_data>(
									network::client_socket{network::connect(result.second, true)},
									result.first,
									conn);
						};

						if (conn->host_ == nullptr)
							connect_task();
						else
						{
							conn->host_->host_registration_.set_cleanup(connect_task);
							epoll_.schedule_cleanup(conn->host_->host_registration_);
						}
					}
					host_to_conn_.erase(result.first);
				}
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
//	log.print_mask |= utils::verbose;
	log(utils::debug) << "debug works\n";

	proxy_server server{network::make_local_endpoint(2539), 3};
	server.run();

	return 0;
}