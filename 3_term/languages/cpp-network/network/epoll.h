#ifndef CPP_NETWORK_EPOLL_H
#define CPP_NETWORK_EPOLL_H

#include <functional>
#include <set>
#include "file_descriptor.h"

namespace network
{
	using callback = std::function<void()>;

	class epoll_registration
	{
		friend class epoll;

		callback on_read, on_write, on_close, cleanup;

		int raw_fd;

	public:
		explicit epoll_registration(file_descriptor const &fd) noexcept;

		void set_on_read(callback on_read);

		void set_on_write(callback on_write);

		void set_on_close(callback on_close);

		void set_cleanup(callback cleanup);

		void unset_on_read();

		void unset_on_write();

		void unset_on_close();

		void unset_cleanup();
	};

	class epoll : public base_descriptor_resource
	{
		bool is_running = false;

		std::set<epoll_registration const *> cleanup_set;

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
