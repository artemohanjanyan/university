#include "request.h"

namespace network
{
	namespace http
	{
		request_line::request_line(request_type method,
		                           std::string const &request_uri,
		                           std::string const &http_version)
				: method(method)
				, request_uri(request_uri)
				, http_version(http_version)
		{}

		request_type request_line::get_method() const noexcept
		{
			return method;
		}

		std::string const &request_line::get_request_uri() const noexcept
		{
			return request_uri;
		}

		std::string const &request_line::get_http_version() const noexcept
		{
			return http_version;
		}


		request::request(request_line const &request_line_,
		                 std::unordered_map<std::string, std::string> const &headers)
				: request_line_(request_line_)
				, headers(headers)
		{}

		request_line const &request::get_request_line() const noexcept
		{
			return request_line_;
		}

		std::unordered_map<std::string, std::string> const &request::get_headers() const noexcept
		{
			return headers;
		}


		std::string to_string(request_type request_type_)
		{
			return {};
		}

		std::string to_string(request_line const &request_line)
		{
			return {};
		}

		std::string to_string(request const &request_)
		{
			return {};
		}
	}
}
