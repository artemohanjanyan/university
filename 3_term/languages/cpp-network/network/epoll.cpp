#include "epoll.h"
#include "network_exception.h"
#include "utils/log.h"

#include <sys/epoll.h>

#include <forward_list>
#include <iostream>

namespace network
{
	epoll_registration::epoll_registration(file_descriptor const *fd, epoll *ep) noexcept
			: fd{fd}
			, ep{ep}
	{
		on_close = [this] {
			this->ep->schedule_cleanup(*this);
		};
	}

	epoll_registration::~epoll_registration()
	{
		ep->remove(*this);
	}

	epoll_registration &epoll_registration::set_on_read(callback on_read)
	{
		this->on_read = on_read;
		return *this;
	}

	epoll_registration &epoll_registration::set_on_write(callback on_write)
	{
		this->on_write = on_write;
		return *this;
	}

	epoll_registration &epoll_registration::set_on_close(callback on_close)
	{
		this->on_close = on_close;
		return *this;
	}

	epoll_registration &epoll_registration::set_cleanup(callback cleanup)
	{
		this->cleanup = cleanup;
		return *this;
	}

	void epoll_registration::update()
	{
		ep->update(*this);
	}

	epoll::epoll(file_descriptor &&fd) noexcept
			: base_descriptor_resource{std::move(fd)}
	{}

	epoll::epoll()
			: epoll{file_descriptor{check_return_code(::epoll_create1(0))}}
	{}

	epoll::epoll(epoll &&rhs) noexcept
			: epoll{std::move(rhs.fd)}
	{}

	void epoll::add(epoll_registration &registration)
	{
		epoll_event event;
		event.data.ptr = static_cast<void *>(&registration);
		event.events = (registration.on_read != nullptr ? EPOLLIN : 0u) |
		               (registration.on_write != nullptr ? EPOLLOUT : 0u) |
		               EPOLLRDHUP;
		check_return_code(
				epoll_ctl(this->fd.get_raw_fd(), EPOLL_CTL_ADD, registration.fd->get_raw_fd(), &event));
	}

	void epoll::update(epoll_registration &registration)
	{
		epoll_event event;
		event.data.ptr = static_cast<void *>(&registration);
		event.events = (registration.on_read != nullptr ? EPOLLIN : 0u) |
		               (registration.on_write != nullptr ? EPOLLOUT : 0u) |
		               EPOLLRDHUP;
		check_return_code(
				epoll_ctl(this->fd.get_raw_fd(), EPOLL_CTL_MOD, registration.fd->get_raw_fd(), &event));
	}

	void epoll::remove(epoll_registration &registration)
	{
		check_return_code(
				epoll_ctl(this->fd.get_raw_fd(), EPOLL_CTL_DEL, registration.fd->get_raw_fd(), nullptr));
	}

	void epoll::schedule_cleanup(epoll_registration &registration)
	{
		cleanup_set.insert(&registration);
	}

	void epoll::run()
	{
		is_running = true;
		while (is_running)
		{
			std::array<epoll_event, 10> events;
			int event_n = epoll_wait(fd.get_raw_fd(), events.begin(), static_cast<int>(events.size()), -1);
			check_return_code(event_n);

			for (int event_i = 0; event_i < event_n && is_running; ++event_i)
			{
				epoll_registration *registration = static_cast<epoll_registration *>(events[event_i].data.ptr);
				try
				{
					if (events[event_i].events & EPOLLIN)
					{
						#ifdef CPP_NETWORK_SOCKET_DEBUG
						log(utils::verbose) << "can read from " << *registration->fd << "\n";
						#endif
						registration->on_read();
					}
					if (events[event_i].events & EPOLLOUT)
					{
						#ifdef CPP_NETWORK_SOCKET_DEBUG
						log(utils::verbose) << "can write to " << *registration->fd << "\n";
						#endif
						registration->on_write();
					}
					if (events[event_i].events & ~(EPOLLIN | EPOLLOUT))
					{
						#ifdef CPP_NETWORK_SOCKET_DEBUG
						log(utils::verbose) << "closed on " << *registration->fd << "\n";
						#endif
						registration->on_close();
					}
				}
				catch (std::exception &exception)
				{
					log(utils::error) << "Exception thrown while processing file descriptor " <<
							*registration->fd << ": " << exception.what() << "\n";
					this->schedule_cleanup(*registration);
				}
			}

			for (auto registration : cleanup_set)
				if (registration->cleanup != nullptr)
					try
					{
						registration->cleanup();
					}
					catch (std::exception &exception)
					{
						log(utils::error) << "Exception during cleanup on file descriptor " <<
								*registration->fd << ": " << exception.what() << "\n";
					}
			cleanup_set.clear();
		}
	}

	void epoll::soft_stop() noexcept
	{
		is_running = false;
	}
}
