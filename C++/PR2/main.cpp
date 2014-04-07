#include <iostream>
#include <map>
#include <memory>
#include <utility>

// bimap -- двусторонний map. Поиск по ключу и по значению
// вставка -- за логарфим
// поиск -- за логарифм

using std::make_pair;
using std::cout;
using std::endl;

typedef std::pair<int, int> ii;

struct IteratorToItself;
typedef std::map<int, IteratorToItself> Map;

struct IteratorToItself {

    IteratorToItself()
    {
    }

    IteratorToItself(Map::iterator const &it)
    {
        iter.reset(new Map::iterator(it));
    }
    
    std::shared_ptr<Map::iterator> iter;
};

class bimap {
private:
    Map a, b;

public:
    bimap()
    {
    }

    void insert(ii const &k)
    {
        a[k.first];
        b[k.second];

        auto it1 = a.find(k.first);
        auto it2 = b.find(k.second);

        it1->second = it2;
        it2->second = it1;
    }

    int operator[](int key)
    {
        auto it = a.find(key);
        if (it != a.end())
            return (*it->second.iter)->first;

        it = b.find(key);
        if (it != b.end())
            return (*it->second.iter)->first;

        return -1;
    }
};

int main()
{
    bimap b;
    b.insert(make_pair(1, 1));
    b.insert(make_pair(2, 4));
    b.insert(make_pair(3, 9));

    cout << b[1] << endl;
    cout << b[2] << endl;
    cout << b[3] << endl;
    cout << b[4] << endl;
    cout << b[9] << endl;

    cout << endl;
    return 0;
}
