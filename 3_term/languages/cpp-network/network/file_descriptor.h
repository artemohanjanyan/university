#ifndef CPP_NETWORK_FILE_DESCRIPTOR_H
#define CPP_NETWORK_FILE_DESCRIPTOR_H


namespace network
{


	class file_descriptor
	{
		int fd;

	public:
		explicit file_descriptor(int fd);
		file_descriptor(file_descriptor &&rhs);
		~file_descriptor();

		int get_raw_fd() const noexcept;
	};


}


#endif //CPP_NETWORK_FILE_DESCRIPTOR_H
