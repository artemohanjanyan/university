#include <network/socket.h>
#include <network/epoll.h>
#include <network/http/request.h>
#include <network/http/request_parser.h>
#include <utils/string_buffer.h>
#include <utils/log.h>

#include <memory>
#include <map>
#include <iostream>

utils::log log{std::cout};

void push_buffer(utils::string_buffer *buffer,
                 network::client_socket *dst,
                 network::epoll_registration *dst_registration)
{
	if (buffer->is_empty())
		dst_registration->set_on_write([buffer, dst, dst_registration] {
			buffer->pop(dst->write(buffer->top()));
			if (buffer->is_empty())
				dst_registration->set_on_write(nullptr).update();
		}).update();
}

struct connection
{
	struct host_data
	{
		network::client_socket host_;
		network::epoll_registration host_registration_;
		std::string host_name_;

		host_data(network::client_socket &&host, std::string host_name, connection *outer);
	};

	network::client_socket client_;
	network::epoll_registration client_registration_;
	network::http::request_parser parser_;
	network::http::request_parser_registration parser_registration_;

	std::unique_ptr<host_data> host_;
	utils::string_buffer write_buffer_;
	utils::string_buffer read_buffer_;

	network::epoll &epoll_;
	std::map<int, std::unique_ptr<connection>> &map_;

	network::callback host_write_;

	connection(network::client_socket &&client_init,
	           network::epoll &epoll_init,
	           std::map<int, std::unique_ptr<connection>> &map_init)
			: client_{std::move(client_init)}
			, client_registration_{&this->client_.get_fd(), &epoll_init}
			, parser_{}
			, parser_registration_{}
			, epoll_{epoll_init}
			, map_{map_init}
	{
		client_registration_.set_on_read([this] {
			parser_.parse(client_.read());
		});

		parser_.register_consumer(&parser_registration_);

		parser_registration_.set_request_consumer([this](network::http::request request) {
			std::string new_host_name = request.headers().at("Host");
			if (host_ == nullptr || host_->host_name_ != new_host_name)
			{
				decltype(auto) hosts = network::get_hosts(new_host_name);
				host_ = std::make_unique<host_data>(
						network::client_socket{std::move(hosts)},
						std::move(new_host_name),
						this);
				write_buffer_ = {};
				read_buffer_ = {};
			}
			push_buffer(&write_buffer_, &host_->host_, &host_->host_registration_);
			log(utils::verbose) << to_string(request);
			write_buffer_.push(to_string(request));
		});

		parser_registration_.set_chunk_consumer([this](std::string chunk) {
			log(utils::verbose) << chunk;
			write_buffer_.push(std::move(chunk));
		});

		client_registration_.set_cleanup([this] {
			map_.erase(client_.get_fd().get_raw_fd());
		});
	}
};

connection::host_data::host_data(network::client_socket &&host, std::string host_name, connection *outer)
		: host_{std::move(host)}
		, host_registration_{&host_.get_fd(), &outer->epoll_}
		, host_name_{std::move(host_name)}
{
	host_registration_.set_on_read([this, outer] {
		push_buffer(&outer->read_buffer_, &outer->client_, &outer->client_registration_);
		outer->read_buffer_.push(host_.read());
	});

	host_registration_.set_on_close([this, outer] {
		outer->epoll_.schedule_cleanup(outer->client_registration_);
	});

	host_registration_.set_cleanup([this, outer] {
		outer->map_.erase(outer->client_.get_fd().get_raw_fd());
	});

	outer->epoll_.add(host_registration_);
}

int main()
{
//	network::log.print_mask |= utils::verbose;
//	log.print_mask |= utils::verbose;
	log(utils::debug) << "Debug works\n";
	network::server_socket server{network::make_local_endpoint(2539)};
	network::epoll epoll{};
	network::epoll_registration server_registration{&server.get_fd(), &epoll};
	std::map<int, std::unique_ptr<connection>> map;

	server_registration.set_on_read([&] {
		std::unique_ptr<connection> conn = std::make_unique<connection>(server.accept(), epoll, map);
		epoll.add(conn->client_registration_);
		map.insert(std::make_pair(conn->client_.get_fd().get_raw_fd(), std::move(conn)));
	});

	server_registration.set_cleanup([&] {
		epoll.soft_stop();
	});

	epoll.add(server_registration);
	epoll.run();

	return 0;
}