#include "ThreadPool.hpp"

ThreadPool::ThreadPool(size_t workers)
    : is_running_(true), pool_size_(workers ? workers : boost::thread::hardware_concurrency())
{
    for (size_t i = 0; i < pool_size_; ++i) {
        threads_.create_thread(boost::bind(&ThreadPool::pool_main, this));
    }
}

ThreadPool::~ThreadPool()
{
    stop();
}

void ThreadPool::start_and_wait()
{
    boost::unique_lock<boost::mutex> lock(wait_mutex_);
    waiting_thread_count = 0;
    started = true;
    start_cv_.notify_all();
    wait_cv_.wait(lock, [this] { return waiting_thread_count == pool_size_ && tasks_.empty(); });
    stop();
}

void ThreadPool::stop()
{
    is_running_ = false;
    started = true;
    start_cv_.notify_all();
    tasks_cv_.notify_all();
    threads_.join_all();
}

void ThreadPool::pool_main()
{
    while (is_running_) {
        boost::function<void()> task;
        {
            boost::unique_lock<boost::mutex> lock(tasks_mutex_);

            start_cv_.wait(lock, [this] { return started; });

            ++waiting_thread_count;
            wait_cv_.notify_one();
            tasks_cv_.wait(lock, [this] { return !is_running_ || !tasks_.empty(); });

            if (!is_running_)
                break;
            task = tasks_.front();
            tasks_.pop();
            if (waiting_thread_count)
                --waiting_thread_count;
        }
        task();
    }
}