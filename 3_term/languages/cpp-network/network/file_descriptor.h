#ifndef CPP_NETWORK_FILE_DESCRIPTOR_H
#define CPP_NETWORK_FILE_DESCRIPTOR_H

namespace utils
{
	class log;
}

namespace network
{
	extern utils::log log;

	class file_descriptor
	{
		int fd;

	public:
		explicit file_descriptor(int fd);

		file_descriptor(file_descriptor const &) = delete;

		file_descriptor(file_descriptor &&rhs) noexcept;

		~file_descriptor() noexcept;

		file_descriptor &operator=(file_descriptor &&rhs) noexcept;

		int get_raw_fd() const noexcept;

		bool is_set() const noexcept;
	};

	void make_non_blocking(file_descriptor const &fd);

	class base_descriptor_resource
	{
	protected:
		file_descriptor fd;

	public:
		base_descriptor_resource(file_descriptor &&fd) noexcept;

		file_descriptor const &get_fd() const noexcept;
	};
}

#endif //CPP_NETWORK_FILE_DESCRIPTOR_H
