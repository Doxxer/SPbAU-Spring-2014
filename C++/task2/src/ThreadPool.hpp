#ifndef THREADPOOL_HPP
#define THREADPOOL_HPP

#include <boost/function.hpp>
#include <boost/thread.hpp>
#include <queue>
#include <vector>

using boost::thread_group;
using std::vector;

template <typename R> class ThreadPool {
private:
    std::queue<boost::function<void(R &)>> tasks_;
    boost::thread_group threads_;
    boost::mutex tasks_mutex_;
    boost::mutex wait_mutex_;
    boost::condition_variable tasks_cv_;
    boost::condition_variable wait_cv_;
    boost::condition_variable start_cv_;
    bool is_running_;
    size_t waiting_thread_count = 0;
    size_t pool_size_;
    bool started = false;
    vector<R> result_;

public:
    ThreadPool(size_t workers = 0)
        : is_running_(true), pool_size_(workers ? workers : boost::thread::hardware_concurrency())
    {
        result_.resize(pool_size_);
        for (size_t i = 0; i < pool_size_; ++i) {
            threads_.create_thread(boost::bind(&ThreadPool::pool_main, this, i));
        }
    }

    ~ThreadPool()
    {
        stop();
    }

    vector<R> get_result()
    {
        return result_;
    }

    template <typename Task> void add_task(Task task)
    {
        boost::lock_guard<boost::mutex> lock(tasks_mutex_);
        tasks_.push(boost::function<void(R &)>(task));
        tasks_cv_.notify_one();
    }

    void start_and_wait()
    {
        boost::unique_lock<boost::mutex> lock(wait_mutex_);
        waiting_thread_count = 0;
        started = true;
        start_cv_.notify_all();
        wait_cv_.wait(lock,
                      [this] { return waiting_thread_count == pool_size_ && tasks_.empty(); });
        stop();
    }

    void stop()
    {
        is_running_ = false;
        started = true;
        start_cv_.notify_all();
        tasks_cv_.notify_all();
        threads_.join_all();
    }

private:
    void pool_main(size_t thread_number)
    {
        while (is_running_) {
            boost::function<void(R &)> task;
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
            task(result_[thread_number]);
        }
    }
};

#endif /* end of include guard: THREADPOOL_HPP */
