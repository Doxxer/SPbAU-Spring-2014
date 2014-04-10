#include <iostream>
#include <typeinfo>
#include <type_traits>

struct Base {
};
struct Derived : Base {
};

template <typename... Types> struct CommonType;

template <typename T> struct CommonType<T> {
    using common_type = T;
};

template <typename T1, typename T2> struct CommonType<T1, T2> {
    using common_type = typename std::remove_reference<
        decltype(true ? *static_cast<typename std::remove_reference<T1>::type *>(nullptr)
                      : *static_cast<typename std::remove_reference<T2>::type *>(nullptr))>::type;
};

template <typename T, typename... Other> struct CommonType<T, Other...> {
    using common_type =
        typename CommonType<T, typename CommonType<Other...>::common_type>::common_type;
};

namespace detail {
template <typename T> T &&min_impl(T &&t)
{
    return std::forward<T>(t);
}

template <typename T, typename S> typename CommonType<T, S>::common_type &&min_impl(T &&t, S &&s)
{
    return std::forward<typename CommonType<T, S>::common_type>(t < s ? t : s);
}

template <typename T, typename... Args>
typename CommonType<T, Args...>::common_type &&min_impl(T &&t, Args &&... args)
{
    return std::forward<typename CommonType<T, Args...>::common_type>(
        (t < min_impl(std::forward<Args>(args)...) ? t : min_impl(std::forward<Args>(args)...)));
}
}

template <typename... Args> typename CommonType<Args...>::common_type min(Args &&... args)
{
    return std::forward<typename CommonType<Args...>::common_type>(
        detail::min_impl(std::forward<Args>(args)...));
}

int main()
{
    std::cout << typeid(CommonType<int, unsigned>::common_type).name() << std::endl;
    std::cout << min("23.123", "423", std::string("30")) << std::endl;
    return 0;
}