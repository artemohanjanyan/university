#include "request.h"
#include "request_parser.h"

#include <sstream>
#include <algorithm>

namespace network { namespace http
{
	request_line::request_line(request_type type_,
	                           std::string uri_,
	                           std::string http_version)
			: type_{type_}
			, uri_{std::move(uri_)}
			, http_version_{std::move(http_version)}
	{}

	request_type request_line::type() const noexcept
	{
		return type_;
	}

	std::string const &request_line::uri() const noexcept
	{
		return uri_;
	}

	std::string const &request_line::http_version() const noexcept
	{
		return http_version_;
	}

	request::request(request_line request_line_,
	                 std::unordered_map<std::string, std::string> headers_)
			: line_{std::move(request_line_)}
			, headers_{std::move(headers_)}
	{}

	request_line const &request::line() const noexcept
	{
		return line_;
	}

	std::unordered_map<std::string, std::string> const &request::headers() const noexcept
	{
		return headers_;
	}

	std::string to_string(request_type request_type_)
	{
		switch (request_type_)
		{
			case request_type::get:
				return "GET";
			case request_type::post:
				return "POST";
			default: // to suppress CLion and gcc warnings at the same time
				throw std::runtime_error{"unknown enum value"};
		}
	}

	request_type request_type_from_string(std::string type_str)
	{
		if (type_str == "GET")
			return request_type::get;
		else if (type_str == "POST")
			return request_type::post;
		throw parse_exception{"bad request type \"" + type_str + "\""};
	}

	std::string to_string(request_line const &request_line_)
	{
		return to_string(request_line_.type()) + " "
		       + request_line_.uri() + " "
		       + request_line_.http_version() + "\r\n";
	}

	std::string to_string(request_line const &request_line, std::string host)
	{
		host = "http://" + host;
		std::string uri = request_line.uri();
		if (std::equal(host.begin(), host.end(), uri.begin()))
			uri = uri.substr(host.size());
		return to_string(request_line.type()) + " "
		       + uri + " "
		       + request_line.http_version() + "\r\n";
	}

	std::string to_string(request const &request_)
	{
		std::stringstream buffer;
		buffer << to_string(request_.line());
		for (auto const &header : request_.headers())
			buffer << header.first << ": " << header.second << "\r\n";
		buffer << "\r\n";
		return buffer.str();
	}

	std::string to_string(request const &request, std::string const &host)
	{
		std::stringstream buffer;
		buffer << to_string(request.line(), host);
		for (auto const &header : request.headers())
			buffer << header.first << ": " << header.second << "\r\n";
		buffer << "\r\n";
		return buffer.str();
	}
}}
