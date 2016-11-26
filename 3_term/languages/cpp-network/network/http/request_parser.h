#ifndef CPP_NETWORK_REQUEST_PARSER_H
#define CPP_NETWORK_REQUEST_PARSER_H

#include "request.h"

#include <string>
#include <functional>
#include <vector>

namespace network
{
	namespace http
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
			void set_request_consumer(request_consumer const &request_consumer_);

			void set_chunk_consumer(chunk_consumer const &chunk_consumer_);

			void unset_request_consumer();

			void unset_chunk_consumer();
		};

		class request_parser
		{
			request_parser_registration registration_;

			static const std::vector<char> SPACE;
			static const std::vector<char> COLON;
			static const std::vector<char> CRLF;
			static const std::vector<char> REQUEST_TAIL;

			std::vector<char> buffer_;
			std::vector<char>::const_iterator last_scanned_it_;

			std::vector<char>::const_iterator
			read_until(std::vector<char>::const_iterator begin,
			           std::vector<char>::const_iterator end,
			           std::vector<char> const &stop);

			std::string
			advance_until(std::vector<char>::const_iterator &it,
			              std::vector<char>::const_iterator end,
			              std::vector<char> const &stop);

			bool scan_buffer();

		public:
			request_parser();

			void register_consumer(request_parser_registration const &registration_);

			void parse(std::string str);
		};

		class parse_exception : public std::runtime_error
		{
		public:
			explicit parse_exception(std::string msg);
		};
	}
}

#endif //CPP_NETWORK_REQUEST_PARSER_H
