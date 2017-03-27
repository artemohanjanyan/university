#ifndef CPP_NETWORK_EXECUTOR_H
#define CPP_NETWORK_EXECUTOR_H

#include <queue>
#include <vector>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <atomic>

namespace utils
{
	using task = std::function<void()>;

	template <typename T>
	class blocking_queue
	{
		std::queue<T> queue;
		std::mutex mutex;
		std::condition_variable conditional;

	public:
		void push(T elem)
		{
			std::lock_guard<std::mutex> lock(mutex);
			queue.push(std::move(elem));
			if (queue.size() == 1)
				conditional.notify_one();
		}

		T pop()
		{
			std::unique_lock<std::mutex> lock(mutex);
			while (queue.empty())
				conditional.wait(lock);
			T front = std::move(queue.front());
			queue.pop();
			return front;
		}
	};

	class executor
	{
		blocking_queue<task> queue;
		std::atomic<bool> is_running;
		std::vector<std::thread> threads;

	public:
		executor(size_t n);

		~executor();

		void push_task(task elem);

		void soft_stop();
	};
}

#endif //CPP_NETWORK_EXECUTOR_H
