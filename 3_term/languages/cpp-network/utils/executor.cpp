#include "executor.h"

namespace utils
{
	void executor::stop_and_join()
	{
		soft_stop();
		for (std::thread &thread : threads_)
			thread.join();
	}

	executor::executor(size_t n)
	{
		threads_.reserve(n);
		try
		{
			for (size_t i = 0; i < n; ++i)
				threads_.emplace_back([&]() {
					try
					{
						while (true)
							queue_.pop()();
					}
					catch (std::runtime_error const &e)
					{
						return;
					}
				});
		}
		catch (std::exception const &e)
		{
			stop_and_join();
		}
	}

	void executor::push_task(task elem)
	{
		queue_.push(std::move(elem));
	}

	void executor::soft_stop()
	{
		queue_.interrupt();
	}

	executor::~executor()
	{
		stop_and_join();
	}
}
