#include "log.h"

#include <iostream>

namespace utils
{
	char const *log::reset_style = "\x1B[0m";
	char const *log::error_style = "\x1B[31;1m";
	char const *log::warn_style = "\x1B[31m";
	char const *log::info_style = "\x1B[32;1m";
	char const *log::debug_style = "\x1B[34;1m";
	char const *log::verbose_style = "\x1B[36;1m";
	char const *log::user_style = "\x1B[35;1m";

	char const *log::get_style(log_level level)
	{
		switch (level)
		{
			case error:
				return error_style;
			case warning:
				return warn_style;
			case info:
				return info_style;
			case debug:
				return debug_style;
			case verbose:
				return verbose_style;
			case user:
				return user_style;
			default:
				return reset_style;
		}
	}

	log::log(std::ostream &stream) noexcept
			: stream_(stream)
	{}

	log &log::operator<<(std::string const &msg)
	{
		if (print_mask & current_level_)
			stream_ << get_style(current_level_) << msg << reset_style;
		return *this;
	}

	log &log::operator<<(network::file_descriptor const &fd)
	{
		return *this << std::to_string(fd.get_raw_fd());;
	}

	log &log::operator()(log_level level)
	{
		current_level_ = level;
		return *this;
	}
}