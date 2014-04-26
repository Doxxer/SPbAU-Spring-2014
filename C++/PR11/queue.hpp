#ifndef QUEUE_HPP
#define QUEUE_HPP

#include <queue>
#include <mutex>
#include <condition_variable>

template <typename T> class multithread_queue {
public:
    multithread_queue() : done(false)
    {
    }

    void stop()
    {
        done = true;
        cond_variable.notify_all();
    }

    void push(T const &t)
    {
        std::lock_guard<std::mutex> lock(mutex_);
        queue_.push(t);
        cond_variable.notify_one();
    }

    std::shared_ptr<T> pop()
    {
        std::unique_lock<std::mutex> lock(mutex_);
        cond_variable.wait(lock, [this] { return done || !queue_.empty(); });
        if (done)
            return std::shared_ptr<T>();
        auto value = std::make_shared<T>(queue_.front());
        queue_.pop();
        return value;
    }

private:
    bool done;
    std::queue<T> queue_;
    std::mutex mutex_;
    std::condition_variable cond_variable;
};

#endif /* end of include guard: QUEUE_HPP */
