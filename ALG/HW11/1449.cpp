#include <iostream>
#include <vector>
#include <limits>
#include <utility>
#include <algorithm>

using namespace std;

typedef vector<int> vi;
typedef vector<vi> vvi;

const int INF = numeric_limits<int>::max();

pair<vi, vi> hungarian(vvi const &matrix, int n)
{
    vi u(n, 0), v(n, 0), p(n, -1);
    
    for (int i = 0; i < n; i++) {
        vi links(n, -1), mins(n, INF), used(n, 0);
        int markedI = i, markedJ = -1, j;
        do {
            j = -1;
            for (int j1 = 0; j1 < n; j1++)
                if (!used[j1]) {
                    if (matrix[markedI][j1] - u[markedI] - v[j1] < mins[j1]) {
                        mins[j1] = matrix[markedI][j1] - u[markedI] - v[j1];
                        links[j1] = markedJ;
                    }
                    if (j == -1 || mins[j1] < mins[j])
                        j = j1;
                }

            int delta = mins[j];
            for (int j1 = 0; j1 < n; j1++)
                if (used[j1]) {
                    u[p[j1]] += delta, v[j1] -= delta;
                } else {
                    mins[j1] -= delta;
                }
            u[i] += delta;

            used[j] = 1;
            markedJ = j;
            markedI = p[j];
        } while (markedI != -1);

        for (; links[j] != -1; j = links[j])
            p[j] = p[links[j]];
        p[j] = i;
    }
    return make_pair(u, v);
}

int main()
{
    ios_base::sync_with_stdio(0);
    int n;
    cin >> n;
    vvi m;
    for (int i = 0; i < n; ++i) {
        vi r(n, 0);
        for (int j = 0; j < n; ++j) {
            cin >> r[j];
            r[j] *= -1;
        }
        m.push_back(r);
    }
    pair<vi, vi> uv = hungarian(m, n);

    for (int i = 0; i < n; ++i) {
        cout << -uv.first[i] << " ";
    }
    cout << endl;
    for (int i = 0; i < n; ++i) {
        cout << -uv.second[i] << " ";
    }
    cout << endl;
    return 0;
}