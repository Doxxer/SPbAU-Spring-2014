#ifndef THREADPOOL_HPP
#define THREADPOOL_HPP

#include <boost/function.hpp>
#include <boost/thread.hpp>
#include <boost/filesystem.hpp>
#include <queue>
#include <vector>
#include "Utilities.hpp"

namespace fs = boost::filesystem;
using boost::thread_group;
using std::vector;

class ThreadedTaskQueue {
private:
    std::queue<boost::function<void(vector<fs::path> &, suffixies &)>> task_queue_;
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

    vector<vector<fs::path>> pathes_;
    vector<suffixies> suffixies_;

public:
    ThreadedTaskQueue(size_t workers = 0)
        : is_running_(true), pool_size_(workers ? workers : boost::thread::hardware_concurrency())
    {
        pathes_.resize(pool_size_);
        suffixies_.resize(pool_size_);
        for (size_t i = 0; i < pool_size_; ++i) {
            threads_.create_thread(boost::bind(&ThreadedTaskQueue::work, this, i));
        }
    }

    ~ThreadedTaskQueue()
    {
        stop();
    }

    size_t getThreadCount()
    {
        return pool_size_;
    }

    vector<fs::path> const &getPathes(size_t thread_number)
    {
        return pathes_[thread_number];
    }

    suffixies &getSuffixies(size_t thread_number)
    {
        return suffixies_[thread_number];
    }

    void add_task(boost::function<void(vector<fs::path> &, suffixies &)> task)
    {
        boost::lock_guard<boost::mutex> lock(tasks_mutex_);
        task_queue_.push(task);
        tasks_cv_.notify_one();
    }

    void wait()
    {
        boost::unique_lock<boost::mutex> lock(wait_mutex_);
        waiting_thread_count = 0;
        started = true;
        start_cv_.notify_all();
        wait_cv_.wait(lock,
                      [this] { return waiting_thread_count == pool_size_ && task_queue_.empty(); });
        stop();
    }

private:
    void stop()
    {
        is_running_ = false;
        tasks_cv_.notify_all();
        threads_.join_all();
    }

    void work(size_t thread_number)
    {
        while (is_running_) {
            boost::function<void(vector<fs::path> &, suffixies &)> task;
            {
                boost::unique_lock<boost::mutex> lock(tasks_mutex_);

                while (!started) {
                    start_cv_.wait(lock);
                }

                ++waiting_thread_count;
                wait_cv_.notify_one();
                tasks_cv_.wait(lock, [this] { return !is_running_ || !task_queue_.empty(); });

                if (!is_running_)
                    return;
                task = task_queue_.front();
                task_queue_.pop();
                if (waiting_thread_count)
                    --waiting_thread_count;
            }
            task(pathes_[thread_number], suffixies_[thread_number]);
        }
    }
};

#endif /* end of include guard: THREADPOOL_HPP */
