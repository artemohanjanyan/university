#include "executor.h"

namespace utils
{
	executor::executor(size_t n)
	{
		threads.reserve(n);
		for (size_t i = 0; i < n; ++i)
			threads.push_back(std::thread{[&]() {
				while (is_running)
					queue.pop()();
			}});
	}

	void executor::push_task(task elem)
	{
		queue.push(elem);
	}

	void executor::soft_stop()
	{
		is_running = false;
	}

	executor::~executor()
	{
		soft_stop();
		for (std::thread &thread : threads)
			thread.join();
	}
}
