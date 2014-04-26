#include <iostream>
#include <vector>
#include <algorithm>
#include <future>
#include <queue>
#include <thread>

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
        std::for_each(threads.begin(), threads.end(), std::mem_fun_ref(&std::thread::join));
    }

    template <typename Fret, typename... Fargs>
    std::future<Fret> submit(std::function<Fret(Fargs...)> f, Fargs... args)
    {
        auto ptr = std::make_shared<std::packaged_task<Fret(Fargs...)> >(f);
        std::lock_guard<std::mutex> lock(mutex);
        task_queue.push([=]() { ptr->operator()(args...); });
        return ptr->get_future();
    }

private:
    std::queue<std::function<void()> > task_queue;
    std::vector<std::thread> threads;
    bool done;
    std::mutex mutex;

    void work()
    {
        while (!done) {
            std::lock_guard<std::mutex> lock(mutex);
            if (!task_queue.empty()) {
                auto task = task_queue.front();
                task_queue.pop();
                task();
            }
        }
    }
};

int foo(int a)
{
    ++a;
    std::cout << a << std::endl;
    return a;
}

int main()
{
    ThreadPool tp(10);
    for (int i = 0; i < 20; ++i) {
        tp.submit(std::function<int(int)>(foo), i * 20);
    }

    return 0;
}
