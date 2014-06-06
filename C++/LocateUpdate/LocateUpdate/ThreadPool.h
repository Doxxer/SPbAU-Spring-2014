#ifndef __ThreadPool_H_
#define __ThreadPool_H_

#include <boost/thread/thread.hpp>
#include <boost/function.hpp>
#include <queue>

using boost::thread_group;

class ThreadPool {
private:
    std::queue<boost::function<void()>> tasks_;
    boost::thread_group threads_;
    boost::mutex mutex_;
    boost::mutex mutex_1;
    boost::condition_variable task_queue_conditional_;
    boost::condition_variable w_conditional_;
    boost::condition_variable start_conditional_;
    bool running_;
    size_t w = 0;
    size_t workers_;
    bool go = false;

public:
    ThreadPool(size_t workers = 0) : running_(true), workers_(workers ? workers : boost::thread::hardware_concurrency()) {
        for (size_t i = 0; i < workers_; ++i) {
            threads_.create_thread(boost::bind(&ThreadPool::pool_main, this));
        }
    }


    template<typename TTask>
    void add_task(TTask task) {
        boost::unique_lock<boost::mutex> lock(mutex_);
        tasks_.push(boost::function<void()>(task));
        task_queue_conditional_.notify_one();
    }

    void wait_and_stop() {
        boost::unique_lock<boost::mutex> lock(mutex_1);
        w = 0;
        go = true;
        start_conditional_.notify_all();
        w_conditional_.wait(lock, [this] { return w == workers_ && tasks_.empty(); });
        running_ = false;
        task_queue_conditional_.notify_all();
        threads_.join_all();
    }

private:
    void pool_main() {
        while (running_) {
            boost::function<void()> task;
            {
                boost::unique_lock<boost::mutex> lock(mutex_);

                start_conditional_.wait(lock, [this] { return go; });

                w = std::min(w + 1, workers_);
                w_conditional_.notify_one();
                task_queue_conditional_.wait(lock, [this] {
                    return !running_ || !tasks_.empty();
                });

                if (!running_)
                    break;
                task = tasks_.front();
                tasks_.pop();
                if (w)
                    --w;
            }
            task();
        }
    }
};


#endif //__ThreadPool_H_
