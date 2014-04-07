#include <iostream>
#include <vector>
#include <algorithm>
#include <iterator>
#include <utility>
#include <functional>

struct Odd {
    template <typename T> bool operator()(T const &value)
    {
        return (value % 2 == 1);
    }
} odd_functor;

struct Plus10 {
    template <typename T> int operator()(T const &value)
    {
        return value + 10;
    }
} plus10_functor;

template <typename C, typename F> C filter(C const &container, F func)
{
    C result;
    std::remove_copy_if(container.begin(), container.end(), std::back_inserter(result), func);
    return result;
}

template <class T, class Alloc, template <typename, typename> class C, typename F>
C<typename std::result_of<F(T)>::type, Alloc> map(C<T, Alloc> const &container, F func)
{
    typedef typename std::result_of<F(T)>::type func_result_type;
    C<func_result_type, Alloc> result;

    for (auto it = container.begin(); it != container.end(); ++it) {
        result.push_back(func(*it));
    }
    return result;
}

template <typename C1, typename C2>
std::vector<std::pair<typename C1::value_type, typename C2::value_type> > zip(C1 const &container1,
                                                                              C2 const &container2)
{
    std::vector<std::pair<typename C1::value_type, typename C2::value_type> > result;

    auto it1 = container1.begin();
    auto it2 = container2.begin();
    for (; it1 != container1.end() && it2 != container2.end(); ++it1, ++it2) {
        result.push_back(std::make_pair(*it1, *it2));
    }

    return result;
}

template <typename T> void print(T const &c)
{
    for (auto it = c.begin(); it != c.end(); ++it)
        std::cout << *it << " ";
    std::cout << std::endl;
}

int main()
{
    std::vector<int> a = { 1, 2, 3, 4, 5 };
    std::vector<double> b = { 1.3, 4.1, 9.2, 16.5, 25.9 };

    auto filtered = filter(a, odd_functor);
    auto mapped = map(a, plus10_functor);
    auto zipped = zip(a, b);

    print(filtered);
    print(mapped);

    for (auto it = zipped.begin(); it != zipped.end(); ++it)
        std::cout << "<" << it->first << ", " << it->second << "> ";
    std::cout << std::endl;

    return 0;
}
