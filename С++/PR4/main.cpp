#include <vector>
#include <list>
#include <iostream>

using std::vector;
using std::list;
using std::cout;
using std::endl;

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef list<vi> lvi;
typedef list<vvi> lvvi;

struct functor {

    void operator()(int i)
    {
        cout << ' ' << i;
    }

    template <typename T, typename Alloc, template <typename, typename> class C>
    void operator()(C<T, Alloc> const &a)
    {
        for_each(a.begin(), a.end(), functor());
        cout << endl;
    }
};

int main()
{
    vi a(5, 10);
    lvi b(3, vi(10, 1));
    lvvi c(3, vvi(10, vi(5, 1)));    
    
    functor f;

    for_each(a.begin(), a.end(), f);
    cout << endl;
    for_each(b.begin(), b.end(), f);
    cout << endl;
    for_each(c.begin(), c.end(), f);
    cout << endl;
    
    return 0;
}
