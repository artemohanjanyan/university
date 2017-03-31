#ifndef CPP_NETWORK_EPOLL_H
#define CPP_NETWORK_EPOLL_H

#include "file_descriptor.h"

#include <functional>
#include <set>

namespace network
{
	using callback = std::function<void()>;

	class epoll;

	class epoll_registration
	{
		friend class epoll;

		callback on_read_, on_write_, on_close_, cleanup_;
		file_descriptor const *fd_;
		epoll *ep_;

	public:
		epoll_registration(file_descriptor const *fd, epoll *ep) noexcept;

		~epoll_registration();

		epoll_registration &set_on_read(callback on_read);

		epoll_registration &set_on_write(callback on_write);

		epoll_registration &set_on_close(callback on_close);

		epoll_registration &set_cleanup(callback cleanup);

		void update();
	};

	class epoll : public base_descriptor_resource
	{
		bool is_running_ = false;

		std::set<epoll_registration *> cleanup_set_;

	public:
		epoll(file_descriptor &&fd) noexcept;

		epoll();

		epoll(epoll &&rhs) noexcept;

		void add(epoll_registration &registration);

		void update(epoll_registration &registration);

		void remove(epoll_registration &registration);

		void schedule_cleanup(epoll_registration &registration);

		void run();

		void soft_stop() noexcept;
	};
}

#endif //CPP_NETWORK_EPOLL_H
