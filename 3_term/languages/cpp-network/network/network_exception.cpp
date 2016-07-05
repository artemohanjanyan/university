#include "network_exception.h"

namespace network
{
	network_exception::network_exception(std::string msg) : runtime_error(msg)
	{
	}
}
