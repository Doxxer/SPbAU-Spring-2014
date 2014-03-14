#include <exception>
#include <stdexcept>

template <typename T> class Expected {
    union data {
        T t_;
        std::exception_ptr ex_;

        data()
        {
        }
        
        data(T t) : t_(t)
        {
        }
        
        data(data const &d) // : t_(d.t_), ex_(d.ex_)
        {
        }
        
        ~data()
        {
        }
    } data_;

    bool valid_;

public:
    Expected() : valid_(false)
    {
    }

    Expected(T const &t) : data_(t), valid_(true)
    {
    }

    Expected(Expected const &e) : data_(e.data_), valid_(e.valid_)
    {
    }

    T &get()
    {
        if (valid())
            return data_.t_;
        else {
            std::rethrow_exception(data_.ex_);
        }
    }

    template <typename E> bool has_exception()
    {
        if (valid())
            return false;

        try
        {
            std::rethrow_exception(data_.ex_);
        }
        catch (E const &ex)
        {
            return true;
        }
        catch (...)
        {
            return false;
        }
    }

    bool valid()
    {
        return valid_;
    }

    template <typename E> static Expected<T> from_exception(E ex)
    {
        Expected<T> a;
        a.data_.ex_ = make_exception_ptr(ex);
        a.valid_ = false;
        return a;
    }
};

Expected<int> foo(bool f)
{
    if (f)
        return Expected<int>::from_exception(std::logic_error("Logic error"));
    else
        return 200;
}

int main()
{
    auto res0 = foo(0);
    auto res1 = foo(1);

    return 0;
}
