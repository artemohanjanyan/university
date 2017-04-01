#include "executor.h"

namespace utils
{
	executor::executor(size_t n)
	{
		threads_.reserve(n);
		for (size_t i = 0; i < n; ++i)
			threads_.push_back(std::thread{[&]() {
				try
				{
					while (true)
						queue_.pop()();
				}
				catch (std::runtime_error &e)
				{
					return;
				}
			}});
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
		soft_stop();
		for (std::thread &thread : threads_)
			thread.join();
	}
}
