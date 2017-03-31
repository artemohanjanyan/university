#include "request_parser.h"

#include <algorithm>
#include <iterator>
#include <cstddef>

namespace network { namespace http
{
	request_parser_registration &
	request_parser_registration::set_request_consumer(request_consumer const &new_request_consumer)
	{
		this->request_consumer_ = new_request_consumer;
		return *this;
	}

	request_parser_registration &
	request_parser_registration::set_chunk_consumer(chunk_consumer const &new_chunk_consumer)
	{
		this->chunk_consumer_ = new_chunk_consumer;
		return *this;
	}

	std::string
	advance_until(std::deque<char>::const_iterator &it,
	              std::deque<char>::const_iterator end,
	              std::string const &stop)
	{
		auto str_end = std::search(it, end, stop.cbegin(), stop.cend());
		if (str_end == end)
			throw parse_exception{"bad request"};
		std::string str = std::string(it, str_end);
		it = str_end;
		it += stop.size();
		return str;
	}

	request_parser::request_parser()
			: buffer_{}
			, last_scanned_i_{0}
			, scanner_{std::make_unique<request_scanner>(this)}
	{}

	void request_parser::register_consumer(request_parser_registration const *registration)
	{
		this->registration_ = registration;
	}

	void request_parser::parse(std::string const &str)
	{
		for (char c : str)
			buffer_.push_back(c);
		decltype(scanner_->scan()) ret;
		while (ret = scanner_->scan(), ret.first)
			if (ret.second != nullptr)
				scanner_ = std::move(ret.second);
	}

	bool request_parser::try_detect(std::string const &stop)
	{
		auto tail_it = std::search(buffer_.cbegin() + last_scanned_i_, buffer_.cend(),
		                           stop.cbegin(), stop.cend());
		if (tail_it == buffer_.end())
		{
			last_scanned_i_ = buffer_.size() -
			                  std::min(static_cast<ptrdiff_t>(stop.size()) - 1,
			                           static_cast<ptrdiff_t>(buffer_.size()));
			return false;
		}
		last_scanned_i_ = (tail_it - buffer_.cbegin()) + stop.size();
		return true;
	}

	void request_parser::shrink_buffer()
	{
		for (size_t i = 0; i < last_scanned_i_; ++i)
			buffer_.pop_front();
		last_scanned_i_ = 0;
	}

	parse_exception::parse_exception(std::string msg)
			: runtime_error(msg)
	{}

	request_parser::scanner::scanner(request_parser *request_parser)
			: request_parser_{request_parser}
	{}

	request_parser::request_scanner::request_scanner(request_parser *request_parser)
			: scanner(request_parser)
	{}

	std::pair<bool, std::unique_ptr<request_parser::scanner>> request_parser::request_scanner::scan()
	{
		if (!request_parser_->try_detect(REQUEST_TAIL))
			return {false, nullptr};

		auto tail_it = request_parser_->buffer_.cbegin() + request_parser_->last_scanned_i_;
		decltype(auto) it = request_parser_->buffer_.cbegin();

		std::string type_str = advance_until(it, tail_it, SPACE);
		request_type type = request_type_from_string(type_str);
		std::string uri = advance_until(it, tail_it, SPACE);
		std::string http_version = advance_until(it, tail_it, CRLF);
		request_line line{type, std::move(uri), std::move(http_version)};

		std::unordered_map<std::string, std::string> headers;
		auto has_header = [&]() -> bool
		{
			auto tmp_it = it + CRLF.size();
			tmp_it = std::min(tmp_it, tail_it);
			return !std::equal(it, tmp_it, CRLF.cbegin(), CRLF.cend());
		};
		while (has_header())
		{
			std::string header_name = advance_until(it, tail_it, COLON);
			std::string header_value = advance_until(it, tail_it, CRLF);
			headers[header_name] = header_value;
		}

		std::unique_ptr<request_parser::scanner> chunk_scanner{};
		if (headers.count(CHUNK_HEADER_NAME))
			chunk_scanner = std::make_unique<request_parser::chunk_scanner>(request_parser_);

		request_parser_->registration_->request_consumer_({std::move(line), std::move(headers)});
		request_parser_->shrink_buffer();

		return {true, std::move(chunk_scanner)};
	}

	request_parser::chunk_scanner::chunk_scanner(request_parser *request_parser)
			: scanner(request_parser)
	{}

	std::pair<bool, std::unique_ptr<request_parser::scanner>> request_parser::chunk_scanner::scan()
	{
		if (chunk_size == 0)
		{
			if (!request_parser_->try_detect(CRLF))
				return {false, nullptr};

			auto it = request_parser_->buffer_.cbegin();
			while (isxdigit(*it))
				++it;
			chunk_size = static_cast<size_t>(
					std::stoi(std::string{request_parser_->buffer_.cbegin(), it}, nullptr, 16));

			if (advance_until(it, request_parser_->buffer_.end(), CRLF) != "")
				throw parse_exception{"Chunk size not followed by CRLF"};

			chunk_size += it - request_parser_->buffer_.cbegin();
			chunk_size += CRLF.size();

			return {true, nullptr};
		}
		else if (request_parser_->buffer_.size() >= chunk_size)
		{
			request_parser_->last_scanned_i_ = chunk_size;
			if (!std::equal(request_parser_->buffer_.cbegin() + (request_parser_->last_scanned_i_ - CRLF.size()),
			                request_parser_->buffer_.cbegin() + request_parser_->last_scanned_i_,
			                CRLF.cbegin(), CRLF.cend()))
				throw parse_exception{"Chunk not followed by CRLF"};

			request_parser_->registration_->chunk_consumer_(std::string{
					request_parser_->buffer_.cbegin(),
					request_parser_->buffer_.cbegin() + request_parser_->last_scanned_i_});
			request_parser_->shrink_buffer();

			if (chunk_size == 1 + 2 * CRLF.size())
				return {true, std::make_unique<request_parser::request_scanner>(request_parser_)};

			chunk_size = 0;
			return {true, nullptr};
		}
		else
			return {false, nullptr};
	}
}}
