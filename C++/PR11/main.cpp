#include <iostream>
#include <vector>
#include <algorithm>
#include <future>
#include <queue>
#include <thread>

#include "queue.hpp"

class ThreadPool {
public:
    ThreadPool(size_t n) : done(false)
    {
        for (size_t i = 0; i < n; ++i) {
            threads.push_back(std::thread(&ThreadPool::work, this, i));
        }
    }

    ~ThreadPool()
    {
        done = true;
        task_queue.stop();
        std::for_each(threads.begin(), threads.end(), std::mem_fun_ref(&std::thread::join));
    }

    template <typename Fret, typename... Fargs>
    std::future<Fret> submit(std::function<Fret(Fargs...)> f, Fargs... args)
    {
        auto ptr = std::make_shared<std::packaged_task<Fret(Fargs...)>>(f);
        task_queue.push([=]() { ptr->operator()(args...); });
        return ptr->get_future();
    }

private:
    multithread_queue<std::function<void()>> task_queue;
    std::vector<std::thread> threads;
    bool done;

    void work(int id)
    {
        while (!done) {
            auto task = task_queue.pop();
            if (task) {
                std::cout << "thread #" << id << " ";
                task->operator()();
            }
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