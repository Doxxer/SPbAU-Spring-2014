#include <iostream>
#include <vector>
#include <algorithm>
#include <vector>
#include <cstdio>
#include <climits>
#include <string>

typedef long long ll;
typedef std::pair<ll, ll> pll;

std::vector<ll> a;
std::vector<pll> t;

void build(int v, int tl, int tr)
{
    if (tl == tr) {
        t[v].first = a[tl];
        t[v].second = a[tl];
    } else {
        int tm = (tl + tr) / 2;
        build(v * 2, tl, tm);
        build(v * 2 + 1, tm + 1, tr);
        t[v].first = std::max(t[v * 2].first, t[v * 2 + 1].first);
    }
}

ll max(int v, int tl, int tr, int l, int r)
{
    if (l > r)
        return LLONG_MIN;

    //     cout << "find at " << v << " " << tl << " " << tr << " " << l << " " << r << endl;

    if (l == tl && r == tr)
        return t[v].first;
    int tm = (tl + tr) / 2;
    return t[v].second +
           std::max(max(v * 2, tl, tm, l, std::min(r, tm)), max(v * 2 + 1, tm + 1, tr, std::max(l, tm + 1), r));
}

void update(int v, int tl, int tr, int l, int r, int add)
{
    if (l > r)
        return;
    // cout << "updating at " << v << " " << tl << " " << tr << " " << l << " " << r << " +" << add
    //      << endl;
    if (l == tl && tr == r) {
        t[v].second += add;
        // cout << "updated  at " << v << " " << tl << " " << tr << " " << l << " " << r << " +"
        //       << add << endl;
    } else {
        int tm = (tl + tr) / 2;
        update(v * 2, tl, tm, l, std::min(r, tm), add);
        update(v * 2 + 1, tm + 1, tr, std::max(l, tm + 1), r, add);
    }
    t[v].first = t[v].second + std::max(t[v * 2].first, t[v * 2 + 1].first);

    //        cout << "rec " << t[v].first << endl;
}

int main()
{
    //freopen("input.txt", "r", stdin);
    //std::cout.sync_with_stdio(false);
    //std::cin.sync_with_stdio(false);

    int n, q, l, r, v;
    std::string s;

    std::cin >> n >> q;
    a.assign(n, 0);
    t.assign(n * 6, std::make_pair(0, 0));

    for (int i = 0; i < n; ++i) {
        std::cin >> a[i];
    }
    build(1, 0, n - 1);
    for (int i = 0; i < q; ++i) {
        std::cin >> s;
        if (s == "max") {
            std::cin >> l >> r;
            std::cout << max(1, 0, n - 1, l - 1, r - 1) << std::endl;
        } else { // add
            std::cin >> l >> r >> v;
            update(1, 0, n - 1, l - 1, r - 1, v);
        }
    }

    // for (int i = 0; i < 4 * n; ++i) {
    //     cout << t[i].first << " " << t[i].second << endl;
    // }

    return 0;
}
