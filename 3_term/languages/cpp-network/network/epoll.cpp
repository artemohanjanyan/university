#include "epoll.h"
#include "network_exception.h"

#include <sys/epoll.h>

#include <forward_list>
#include <iostream>

namespace network
{
	epoll_registration::epoll_registration(file_descriptor const &fd) noexcept : raw_fd{fd.get_raw_fd()}
	{
	}

	void epoll_registration::set_on_read(callback on_read)
	{
		this->on_read = on_read;
	}

	void epoll_registration::set_on_write(callback on_write)
	{
		this->on_write = on_write;
	}

	void epoll_registration::set_on_close(callback on_close)
	{
		this->on_close = on_close;
	}

	void epoll_registration::unset_on_read()
	{
		this->on_read = nullptr;
	}

	void epoll_registration::unset_on_write()
	{
		this->on_write = nullptr;
	}

	void epoll_registration::unset_on_close()
	{
		this->on_close = nullptr;
	}

	epoll::epoll(file_descriptor &&fd) noexcept : base_descriptor_resource{std::move(fd)}
	{
	}

	epoll::epoll() : epoll{file_descriptor{check_return_code(::epoll_create1(0))}}
	{
	}

	epoll::epoll(epoll &&rhs) noexcept : epoll{std::move(rhs.fd)}
	{
	}

	void epoll::add(epoll_registration const &registration)
	{
		epoll_event event;
		event.data.ptr = const_cast<void *>(static_cast<const void *>(&registration));
		event.events = (registration.on_read != nullptr ? EPOLLIN : 0u) |
		               (registration.on_write != nullptr ? EPOLLOUT : 0u) |
		               EPOLLRDHUP;
		check_return_code(
				epoll_ctl(this->fd.get_raw_fd(), EPOLL_CTL_ADD, registration.raw_fd, &event));
	}

	void epoll::update(epoll_registration const &registration)
	{
		epoll_event event;
		event.data.ptr = const_cast<void *>(static_cast<const void *>(&registration));
		event.events = (registration.on_read != nullptr ? EPOLLIN : 0u) |
		               (registration.on_write != nullptr ? EPOLLOUT : 0u) |
		               EPOLLRDHUP;
		check_return_code(
				epoll_ctl(this->fd.get_raw_fd(), EPOLL_CTL_MOD, registration.raw_fd, &event));
	}

	void epoll::remove(epoll_registration const &registration)
	{
		check_return_code(
				epoll_ctl(this->fd.get_raw_fd(), EPOLL_CTL_DEL, registration.raw_fd, nullptr));
	}

	void epoll::run()
	{
		is_stopped = false;
		while (!is_stopped)
		{
			epoll_event events[10];
			int event_n = epoll_wait(fd.get_raw_fd(), events, sizeof events / sizeof events[0], -1);
			check_return_code(event_n);

			std::forward_list<int> closed;

			for (int event_i = 0; event_i < event_n; ++event_i)
			{
				epoll_registration *registration = static_cast<epoll_registration *>(events[event_i].data.ptr);
				if (events[event_i].events & EPOLLIN)
					registration->on_read();
				if (events[event_i].events & EPOLLOUT)
					registration->on_write();
				if (events[event_i].events & ~(EPOLLIN | EPOLLOUT))
					closed.push_front(event_i);
			}

			for (auto event_i : closed)
			{
				epoll_registration *registration = static_cast<epoll_registration *>(events[event_i].data.ptr);
				if (registration->on_close != nullptr)
					registration->on_close();
			}
		}
	}

	void epoll::soft_stop() noexcept
	{
		is_stopped = true;
	}
}
