#include "request_parser.h"

#include <algorithm>
#include <iterator>
#include <cstddef>

namespace network
{
	namespace http
	{
		void request_parser_registration::set_request_consumer(request_consumer const &request_consumer_)
		{
			this->request_consumer_ = request_consumer_;
		}

		void request_parser_registration::set_chunk_consumer(chunk_consumer const &chunk_consumer_)
		{
			this->chunk_consumer_ = chunk_consumer_;
		}

		void request_parser_registration::unset_request_consumer()
		{
			this->request_consumer_ = nullptr;
		}

		void request_parser_registration::unset_chunk_consumer()
		{
			this->chunk_consumer_ = nullptr;
		}

		const std::vector<char> request_parser::SPACE{' '};
		const std::vector<char> request_parser::COLON{':'};
		const std::vector<char> request_parser::CRLF{'\r', '\n'};
		const std::vector<char> request_parser::REQUEST_TAIL{'\r', '\n', '\r', '\n'};

		std::vector<char>::const_iterator
		request_parser::read_until(std::vector<char>::const_iterator begin,
		                           std::vector<char>::const_iterator end,
		                           std::vector<char> const &stop)
		{
			auto it = std::search(begin, end, stop.begin(), stop.end());
			if (it == end)
				throw parse_exception{"bad request"};
			else
				return it;
		}

		std::string
		request_parser::advance_until(std::vector<char>::const_iterator &it,
		                              std::vector<char>::const_iterator end,
		                              std::vector<char> const &stop)
		{
			auto str_end = read_until(it, end, SPACE);
			if (str_end == end)
				throw parse_exception{"bad request"};
			std::string str = std::string(it, str_end);
			it = str_end;
			std::advance(it, stop.size());
			return std::move(str);
		}

		// TODO add support for chunked
		bool request_parser::scan_buffer()
		{
			auto tail_it = std::search(last_scanned_it_, std::vector<char>::const_iterator{buffer_.end()},
			                           REQUEST_TAIL.begin(), REQUEST_TAIL.end());
			if (tail_it == buffer_.end())
			{
				last_scanned_it_ = buffer_.end();
				std::advance(last_scanned_it_, 1 - static_cast<ptrdiff_t>(REQUEST_TAIL.size()));
				return false;
			}

			tail_it += REQUEST_TAIL.size();
			std::vector<char>::const_iterator it = buffer_.begin();

			request_type type;
			std::string uri;
			std::string http_version;
			{
				std::string type_str = advance_until(it, tail_it, SPACE);
				try
				{
					type = request_type_from_string(type_str);
				}
				catch (std::runtime_error &e)
				{
					throw parse_exception{"bad request"};
				}

				uri = advance_until(it, tail_it, SPACE);

				http_version = advance_until(it, tail_it, CRLF);
			}
			request_line line{type, std::move(uri), std::move(http_version)};

			std::unordered_map<std::string, std::string> headers;
			auto loop_condition = [&]() -> bool
			{
				auto tmp_it = it;
				std::advance(tmp_it, CRLF.size());
				tmp_it = std::min(tmp_it, tail_it);
				return !std::equal(it, tmp_it, CRLF.begin(), CRLF.end());
			};
			while (loop_condition())
			{
				std::string header_name = advance_until(it, tail_it, COLON);
				std::string header_value = advance_until(it, tail_it, CRLF);
				headers[header_name] = header_value;
			}

			registration_.request_consumer_({std::move(line), std::move(headers)});
			return true;
		}

		request_parser::request_parser()
				: buffer_{}
				, last_scanned_it_{buffer_.end()}
		{}

		void request_parser::register_consumer(request_parser_registration const &registration_)
		{
			this->registration_ = registration_;
		}

		void request_parser::parse(std::string str)
		{
			ptrdiff_t it_i = last_scanned_it_ - buffer_.begin();
			for (char c : str)
				buffer_.push_back(c);
			last_scanned_it_ = buffer_.begin() + it_i;
			while (!scan_buffer());
		}

		parse_exception::parse_exception(std::string msg)
				: runtime_error(msg)
		{}
	}
}
