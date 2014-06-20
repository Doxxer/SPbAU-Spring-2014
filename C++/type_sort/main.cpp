#include <iostream>
#include <string>

// sample

struct A {
};
struct B : A {
};
struct C : B {
};

struct A1 {
};
struct B1 : A1 {
};
struct C1 : B1 {
};

struct G { };
// ------------ Same_type

template <typename T, typename R> struct same_type {
    static const bool value = false;
};

template <typename T> struct same_type<T, T> {
    static const bool value = true;
};

// ------------ Type list
struct nil {
};

template <typename H, typename T = nil> struct cons {
    typedef H Head;
    typedef T Tail;
};

template <typename T> void print()
{
    std::cout << std::string(typeid(typename T::Head).name()).substr(1) << " ";
    print<typename T::Tail>();
}

template <> void print<nil>()
{
    std::cout << "|" << std::endl;
}

// ---------------- comparator
typedef char YES;
struct NO {
    YES m[2];
};

template <typename D, typename B> struct is_derived_from {
    static YES test(B *);
    static NO test(...);

    static bool const value = sizeof(test((D *)0)) == sizeof(YES);
};

template <typename B> struct is_derived_from<B, B> {
    static bool const value = false;
};

// --------------------- conditional

template <bool cond, typename ifT, typename ifF> struct conditional {
    typedef ifF type;
};

template <typename ifT, typename ifF> struct conditional<true, ifT, ifF> {
    typedef ifT type;
};

// --------------------- Get base

template <typename A, typename B> struct get_base {
    typedef typename conditional<(is_derived_from<A, B>::value), B, A>::type type;
};

template <typename T, typename List> struct find_base {
private:
    typedef typename find_base<T, typename List::Tail>::type base_in_tail;
    typedef typename get_base<T, typename List::Head>::type base_in_head;

public:
    typedef typename get_base<base_in_head, base_in_tail>::type type;
};

template <typename T> struct find_base<T, nil> {
    typedef cons<T> type;
};

// ------------------ extractor

template <typename T, typename List> struct extractor {
    typedef typename conditional<(same_type<T, typename List::Head>::value),
                                 typename List::Tail,
                                 cons<typename List::Head, typename extractor<T, typename List::Tail>::type>>::type
    type;
};

template <typename T> struct extractor<T, nil> {
    typedef nil type;
};

// ------------------ sorter
template <typename List> struct sorter {
private:
    typedef typename find_base<typename List::Head, typename List::Tail>::type first;
    typedef typename extractor<first, List>::type other;

public:
    typedef cons<first, typename sorter<other>::type> type;
};

template <typename T> struct sorter<cons<T>> {
    typedef cons<T> type;
};

// template <> struct sorter<nil> {
//     typedef nil type;
// };

// ------------------ Example

typedef cons<B, cons<C, cons<A1, cons<C1, cons<A, cons<B1>>>>>> MyList;
//typedef cons<A, cons<A1>> MyList;

int main()
{
    print<MyList>();
    std::cout << is_derived_from<B, A>::value << std::endl << "-----" << std::endl;

    print<cons<typename find_base<A, MyList>::type>>();
    print<cons<typename find_base<B, MyList>::type>>();
    print<cons<typename find_base<C, MyList>::type>>();
    print<cons<typename find_base<A1, MyList>::type>>();
    print<cons<typename find_base<B1, MyList>::type>>();
    print<cons<typename find_base<C1, MyList>::type>>();
    print<cons<typename find_base<G, MyList>::type>>();

    std::cout << "extractor: ------- " << std::endl;

    print<typename extractor<A, MyList>::type>();
    print<typename extractor<B, MyList>::type>();
    print<typename extractor<C, MyList>::type>();
    print<typename extractor<A1, MyList>::type>();
    print<typename extractor<B1, MyList>::type>();
    print<typename extractor<C1, MyList>::type>();

    std::cout << " ------- " << std::endl;

    print<typename sorter<MyList>::type>();

    return 0;
}