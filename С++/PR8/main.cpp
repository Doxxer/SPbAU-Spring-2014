#include <iostream>

using namespace std;

typedef char YES;
struct NO {
    YES m[2];
};

template <typename T> struct Checker {
private:
    template <class Z, void (Z::*)() const = &Z::foo> struct wrapper {
    };

    template <typename C> static YES check(wrapper<C> *p);

    template <typename C> static NO check(...);

public:
    static const bool value = sizeof(check<T>(0)) == sizeof(YES);
};

template <bool B> struct Bool {
};

template <> struct Bool<true> {
};

void call_do(Bool<true>);
void call_do(Bool<false>);

struct NullType {
};

template <typename Head, typename Tail> struct TypeList {
    typedef Head head_type;
    typedef Tail tail_type;
};

template <typename... Args> struct ToTypeList {
    typedef NullType type;
};

template <typename Head, typename... Tail> struct ToTypeList<Head, Tail...> {
    typedef TypeList<Head, typename ToTypeList<Tail...>::type> type;
};

template <typename L> struct Derived : public L::head_type, public Derived<typename L::tail_type> {
    void call_do(Bool<true> const &) const
    {
        L::head_type::foo();
    }

    void call_do(Bool<false> const &) const
    {
    }

    void foo() const
    {
        call_do(Bool<Checker<typename L::head_type>::value>());
        Derived<typename L::tail_type>::foo();
    }
};

template <> struct Derived<NullType> {
    void foo() const
    {
    }
};

struct A1 {
    void foo() const
    {
        cout << "A1" << endl;
    }
};
struct A2 {
    void foo() const
    {
        cout << "A2" << endl;
    }
};
struct A3 {
    // void foo() const
    // {
    //     cout << "A3" << endl;
    // }
};
struct A4 {
    void foo() const
    {
        cout << "A4" << endl;
    }
};

int main()
{
    Derived<typename ToTypeList<A1, A2, A3, A4>::type> s;
    s.foo();
    return 0;
}
