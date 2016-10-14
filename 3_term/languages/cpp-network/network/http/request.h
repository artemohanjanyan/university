#ifndef CPP_NETWORK_REQUEST_H
#define CPP_NETWORK_REQUEST_H

#include <string>
#include <unordered_map>
#include <ostream>

namespace network
{
	namespace http
	{
		enum request_type
		{
			get, post
		};


		class request_line
		{
			request_type method;
			std::string request_uri;
			std::string http_version;

		public:
			// TODO perfect forwarding? const qualifiers etc
			request_line(request_type method, std::string const &request_uri, std::string const &http_version);

			request_type get_method() const noexcept;

			std::string const &get_request_uri() const noexcept;

			std::string const &get_http_version() const noexcept;

			friend std::string to_string(request_line const &request_line);
		};


		class request
		{
			request_line request_line_;
			std::unordered_map<std::string, std::string> headers;

		public:
			// TODO perfect forwarding? const qualifiers etc
			request(request_line const &request_line_, std::unordered_map<std::string, std::string> const &headers);

			request_line const &get_request_line() const noexcept;

			std::unordered_map<std::string, std::string> const &get_headers() const noexcept;

			friend std::string to_string(request const &request_);
		};


		std::string to_string(request_type request_type_);

		std::string to_string(request_line const &request_line);

		std::string to_string(request const &request_);
	}
}

#endif //CPP_NETWORK_REQUEST_H
