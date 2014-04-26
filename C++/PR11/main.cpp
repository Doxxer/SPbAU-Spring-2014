#include <iostream>
#include <vector>
#include <algorithm>
#include <future>
#include <queue>
#include <thread>
#include <mutex>
#include <condition_variable>

class ThreadPool {
public:
    ThreadPool(size_t n) : done(false)
    {
        for (size_t i = 0; i < n; ++i) {
            threads.push_back(std::thread(&ThreadPool::work, this));
        }
    }

    ~ThreadPool()
    {
        done = true;
        cond_variable.notify_all();
        std::for_each(threads.begin(), threads.end(), std::mem_fun_ref(&std::thread::join));
    }

    template <typename Fret, typename... Fargs>
    std::future<Fret> submit(std::function<Fret(Fargs...)> f, Fargs... args)
    {
        std::lock_guard<std::mutex> lock(mutex_);
        auto ptr = std::make_shared<std::packaged_task<Fret(Fargs...)>>(f);
        task_queue.push([=]() { ptr->operator()(args...); });
        cond_variable.notify_one();
        return ptr->get_future();
    }

private:
    std::queue<std::function<void()>> task_queue;
    std::vector<std::thread> threads;
    std::condition_variable cond_variable;
    std::mutex mutex_;
    bool done;

    void work()
    {
        while (!done) {
            std::function<void()> task;
            {
                std::unique_lock<std::mutex> lock(mutex_);
                cond_variable.wait(lock, [this] { return done || !task_queue.empty(); });
                if (done)
                    break;
                task = task_queue.front();
                task_queue.pop();
            }
            task();
        }
    }
};

void foo(int a)
{
    std::cout << a << std::endl;
}

int main()
{
    ThreadPool tp(10);
    for (int i = 0; i < 25; ++i) {
        tp.submit(std::function<void(int)>(foo), i * 20).get();
    }

    return 0;
}