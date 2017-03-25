#ifndef CPP_NETWORK_LOG_H
#define CPP_NETWORK_LOG_H

#include "network/file_descriptor.h"

#include <string>

namespace utils
{
	using log_level = int;

	log_level const error = 1;
	log_level const warning = 2;
	log_level const info = 4;
	log_level const debug = 8;
	log_level const verbose = 16;
	log_level const user = 32;

	class log
	{
		static char const *reset_style;
		static char const *error_style;
		static char const *warn_style;
		static char const *info_style;
		static char const *debug_style;
		static char const *verbose_style;
		static char const *user_style;

		static char const *get_style(log_level level);

		std::ostream &stream;
		log_level current_level;

	public:
		log(std::ostream &stream) noexcept;

		log &operator<<(std::string const &msg);

		log &operator<<(network::file_descriptor const &fd);

		log &operator()(log_level level);

		log_level print_mask = error | warning | info
		#ifndef NDEBUG
		| debug
		#endif
		| user;
	};
}

#endif //CPP_NETWORK_LOG_H
