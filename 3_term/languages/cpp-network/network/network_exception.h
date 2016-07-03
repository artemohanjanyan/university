#ifndef CPP_NETWORK_EXCEPTION_H
#define CPP_NETWORK_EXCEPTION_H


#include <errno.h>
#include <string.h>

#include <stdexcept>

namespace network
{


	class network_exception : public std::runtime_error
	{
	public:
		explicit network_exception(std::string msg);
	};


	template<typename T>
	void check_return_code(T code)
	{
		if (code == -1)
			throw network_exception(strerror(errno));
	}


}


#endif //CPP_NETWORK_EXCEPTION_H
