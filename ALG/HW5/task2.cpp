#include <iostream>
#include <vector>
#include <cstdio>

using namespace std;

const int N = 1000010;
const int S = 19;

typedef vector<int> vi;
typedef vector<vi> vvi;
vvi dp;
vi d;

void add(int a, int v)
{
    dp[v][0] = a;
    for (int i = 1; i < S; ++i) {
        dp[v][i] = dp[dp[v][i - 1]][i - 1];
    }
    d[v] = d[a] + 1;
}

int LCA(int v, int u)
{
    if (d[v] > d[u])
        swap(v, u);
    for (int i = S; i >= 0; --i) {
        if (d[u] - d[v] >= (1 << i))
            u = dp[u][i];
    }
    if (v == u)
        return v;

    for (int i = S; i >= 0; --i) {
        if (dp[v][i] != dp[u][i]) {
            v = dp[v][i];
            u = dp[u][i];
        }
    }
    return dp[v][0];
}

int main()
{
    //freopen("input.txt", "r", stdin);

    int k, a, b;
    int t = 1;
    string s;
    cout.sync_with_stdio(false);
    cin.sync_with_stdio(false);

    cin >> k;

    dp.assign(N, vi(S + 1, 1));
    d.assign(N, 0);

    for (int i = 0; i < k; ++i) {
        cin >> s >> a;
        if (s[0] == 'a') {
            add(a, ++t);
        } else {
            cin >> b;
            cout << LCA(a, b) << endl;
        }
    }
    return 0;
}
