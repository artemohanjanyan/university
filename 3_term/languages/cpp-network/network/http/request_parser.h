#ifndef CPP_NETWORK_REQUEST_PARSER_H
#define CPP_NETWORK_REQUEST_PARSER_H

#include "request.h"

#include <functional>

namespace network
{
	namespace http
	{
		using request_consumer = std::function<void(request)>;

		class request_parser
		{

		};
	}
}

#endif //CPP_NETWORK_REQUEST_PARSER_H
