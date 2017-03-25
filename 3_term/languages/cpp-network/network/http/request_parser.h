#ifndef CPP_NETWORK_REQUEST_PARSER_H
#define CPP_NETWORK_REQUEST_PARSER_H

#include "request.h"

#include <string>
#include <functional>
#include <deque>
#include <memory>

namespace network { namespace http
{
	using request_consumer = std::function<void(request)>;
	using chunk_consumer = std::function<void(std::string)>;

	class request_parser;

	class request_parser_registration
	{
		friend class request_parser;

		request_consumer request_consumer_;
		chunk_consumer chunk_consumer_;

	public:
		request_parser_registration &set_request_consumer(request_consumer const &request_consumer_);

		request_parser_registration &set_chunk_consumer(chunk_consumer const &chunk_consumer_);
	};

	const std::string SPACE = " ";
	const std::string COLON = ": ";
	const std::string CRLF = "\r\n";
	const std::string REQUEST_TAIL = CRLF + CRLF;
	const std::string CHUNK_HEADER_NAME = "Transfer-Encoding";

	std::string
	advance_until(std::deque<char>::const_iterator &it,
	              std::deque<char>::const_iterator end,
	              std::deque<char> const &stop);

	class request_parser
	{
		class scanner;

		class request_scanner;

		class chunk_scanner;

		request_parser_registration const *registration_;

		std::deque<char> buffer_;
		size_t last_scanned_i_;

		std::unique_ptr<scanner> scanner_;

		bool try_detect(std::string const &stop);

		void shrink_buffer();

	public:
		request_parser();

		void register_consumer(request_parser_registration const *registration_);

		void parse(std::string const &str);
	};

	class request_parser::scanner
	{
	public:
		request_parser *request_parser_;

		scanner(request_parser *request_parser_);

		virtual ~scanner()
		{}

		virtual std::pair<bool, std::unique_ptr<scanner>> scan() = 0;
	};

	class request_parser::request_scanner : public request_parser::scanner
	{
	public:
		request_scanner(request_parser *request_parser_);

		std::pair<bool, std::unique_ptr<scanner>> scan() override;
	};

	class request_parser::chunk_scanner : public request_parser::scanner
	{
		size_t chunk_size;

	public:
		chunk_scanner(request_parser *request_parser_);

		std::pair<bool, std::unique_ptr<scanner>> scan() override;
	};

	class parse_exception : public std::runtime_error
	{
	public:
		explicit parse_exception(std::string msg);
	};
}}

#endif //CPP_NETWORK_REQUEST_PARSER_H
