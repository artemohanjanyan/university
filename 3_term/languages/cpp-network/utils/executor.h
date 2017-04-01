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
	class blocking_synchronous_queue
	{
		std::queue<T> queue_;
		std::mutex mutex_;
		std::condition_variable conditional_;
		std::atomic<bool> is_interrupted_{false};

	public:
		void push(T elem)
		{
			std::lock_guard<std::mutex> lock(mutex_);
			queue_.push(std::move(elem));
			if (queue_.size() == 1)
				conditional_.notify_one();
		}

		T pop()
		{
			std::unique_lock<std::mutex> lock(mutex_);
			while (queue_.empty() && !is_interrupted_)
				conditional_.wait(lock);
			if (is_interrupted_)
				throw std::runtime_error{"interrupted"};
			T front = std::move(queue_.front());
			queue_.pop();
			return front;
		}

		void interrupt()
		{
			is_interrupted_ = true;
			conditional_.notify_all();
		}
	};

	template <typename T>
	class blocking_queue
	{
		std::queue<T> queue_;
		std::mutex mutex_;

	public:
		void push(T elem)
		{
			std::lock_guard<std::mutex> lock(mutex_);
			queue_.push(std::move(elem));
		}

		T pop()
		{
			std::unique_lock<std::mutex> lock(mutex_);
			T front = std::move(queue_.front());
			queue_.pop();
			return front;
		}
	};

	class executor
	{
		blocking_synchronous_queue<task> queue_;
		std::vector<std::thread> threads_;

	public:
		executor(size_t n);

		~executor();

		void push_task(task elem);

		void soft_stop();
	};
}

#endif //CPP_NETWORK_EXECUTOR_H
