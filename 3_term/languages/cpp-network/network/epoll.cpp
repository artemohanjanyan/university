#include "epoll.h"
#include "network_exception.h"
#include "utils/log.h"

#include <sys/epoll.h>

#include <forward_list>

namespace network
{
	epoll_registration::epoll_registration(file_descriptor const *fd, epoll *ep) noexcept
			: fd_{fd}
			, ep_{ep}
	{
		on_close_ = [this] {
			this->ep_->schedule_cleanup(*this);
		};
		ep->add(*this);
	}

	epoll_registration::~epoll_registration()
	{
		ep_->remove(*this);
	}

	epoll_registration &epoll_registration::set_on_read(callback on_read)
	{
		this->on_read_ = on_read;
		return *this;
	}

	epoll_registration &epoll_registration::set_on_write(callback on_write)
	{
		this->on_write_ = on_write;
		return *this;
	}

	epoll_registration &epoll_registration::set_on_close(callback on_close)
	{
		this->on_close_ = on_close;
		return *this;
	}

	epoll_registration &epoll_registration::set_cleanup(callback cleanup)
	{
		this->cleanup_ = cleanup;
		return *this;
	}

	void epoll_registration::update()
	{
		ep_->update(*this);
	}

	epoll::epoll(file_descriptor &&fd) noexcept
			: base_descriptor_resource{std::move(fd)}
	{}

	epoll::epoll()
			: epoll{file_descriptor{check_return_code(::epoll_create1(0))}}
	{}

	epoll::epoll(epoll &&rhs) noexcept
			: epoll{std::move(rhs.fd_)}
	{}

	void epoll::add(epoll_registration &registration)
	{
		epoll_event event;
		event.data.ptr = static_cast<void *>(&registration);
		event.events = (registration.on_read_ != nullptr ? EPOLLIN : 0u) |
		               (registration.on_write_ != nullptr ? EPOLLOUT : 0u) |
		               EPOLLRDHUP;
		check_return_code(
				epoll_ctl(this->fd_.get_raw_fd(), EPOLL_CTL_ADD, registration.fd_->get_raw_fd(), &event));
	}

	void epoll::update(epoll_registration &registration)
	{
		epoll_event event;
		event.data.ptr = static_cast<void *>(&registration);
		event.events = (registration.on_read_ != nullptr ? EPOLLIN : 0u) |
		               (registration.on_write_ != nullptr ? EPOLLOUT : 0u) |
		               EPOLLRDHUP;
		check_return_code(
				epoll_ctl(this->fd_.get_raw_fd(), EPOLL_CTL_MOD, registration.fd_->get_raw_fd(), &event));
	}

	void epoll::remove(epoll_registration &registration)
	{
		check_return_code(
				epoll_ctl(this->fd_.get_raw_fd(), EPOLL_CTL_DEL, registration.fd_->get_raw_fd(), nullptr));
	}

	void epoll::schedule_cleanup(epoll_registration &registration)
	{
		cleanup_set_.insert(&registration);
	}

	void epoll::run()
	{
		is_running_ = true;
		while (is_running_)
		{
			std::array<epoll_event, 10> events;
			int event_n = epoll_wait(fd_.get_raw_fd(), events.begin(), static_cast<int>(events.size()), -1);
			if (event_n == -1)
			{
				if (errno != EINTR)
					check_return_code(event_n);
			}

			for (int event_i = 0; event_i < event_n && is_running_; ++event_i)
			{
				epoll_registration *registration = static_cast<epoll_registration *>(events[event_i].data.ptr);
				try
				{
					if (events[event_i].events & EPOLLIN)
					{
						#ifdef CPP_NETWORK_SOCKET_DEBUG
						log(utils::verbose) << "can read from " << *registration->fd_ << "\n";
						#endif
						registration->on_read_();
					}
					if (events[event_i].events & EPOLLOUT)
					{
						#ifdef CPP_NETWORK_SOCKET_DEBUG
						log(utils::verbose) << "can write to " << *registration->fd_ << "\n";
						#endif
						registration->on_write_();
					}
					if (events[event_i].events & ~(EPOLLIN | EPOLLOUT))
					{
						#ifdef CPP_NETWORK_SOCKET_DEBUG
						log(utils::verbose) << "closed on " << *registration->fd_ << "\n";
						#endif
						registration->on_close_();
					}
				}
				catch (std::exception &exception)
				{
					log(utils::error) << "Exception thrown while processing file descriptor " <<
							*registration->fd_ << ": " << exception.what() << "\n";
					this->schedule_cleanup(*registration);
				}
			}

			for (auto registration : cleanup_set_)
				if (registration->cleanup_ != nullptr)
					try
					{
						registration->cleanup_();
					}
					catch (std::exception &exception)
					{
						log(utils::error) << "Exception during cleanup on file descriptor " <<
								*registration->fd_ << ": " << exception.what() << "\n";
					}
			cleanup_set_.clear();
		}
	}

	void epoll::soft_stop() noexcept
	{
		is_running_ = false;
	}
}
