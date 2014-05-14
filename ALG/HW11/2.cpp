#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>

////////////////////////////
using namespace std;
typedef vector<int> vi;
typedef vector<vi> vvi;
const int INF = 1000000000;
////////////////////////////

struct edge {
    int b, cap, flow;
};

vector<edge> edges;
vvi g, games;
vi dist, ptr;
int s;
int n, t;
vi w, r;

void add_edge(int a, int b, int cap)
{
    g[a].push_back((int)edges.size());
    edges.push_back((edge) { b, cap, 0 });
    g[b].push_back((int)edges.size());
    edges.push_back((edge) { a, 0, 0 });
}

// thanks to http://e-maxx.ru/algo/dinic
bool bfs()
{
    dist.assign(n, -1);
    dist[s] = 0;
    queue<int> q;
    q.push(s);
    while (!q.empty() && dist[t] == -1) {
        int v = q.front();
        q.pop();
        for (size_t i = 0; i < g[v].size(); i++) {
            int id = g[v][i], to = edges[id].b;
            if (dist[to] == -1 && edges[id].flow < edges[id].cap) {
                q.push(to);
                dist[to] = dist[v] + 1;
            }
        }
    }
    return dist[t] != -1;
}

// thanks to http://e-maxx.ru/algo/dinic
int dfs(int v, int flow)
{
    if (!flow)
        return 0;
    if (v == t)
        return flow;
    for (; ptr[v] < (int)g[v].size(); ++ptr[v]) {
        int id = g[v][ptr[v]], to = edges[id].b;
        if (dist[to] != dist[v] + 1)
            continue;
        int pushed = dfs(to, min(flow, edges[id].cap - edges[id].flow));
        if (pushed) {
            edges[id].flow += pushed;
            edges[id ^ 1].flow -= pushed;
            return pushed;
        }
    }
    return 0;
}

int algorithmDinica()
{
    int flow = 0;
    while (true) {
        if (!bfs())
            break;
        ptr.assign(n, 0);
        while (int pushed = dfs(s, INF))
            flow += pushed;
    }
    return flow;
}

int main()
{
    cin >> n;
    s = 0;
    t = n + 1;
    int size = n + 1;

    g.resize(size + n * n);
    w.resize(size);
    r.resize(size);
    for (int i = 1; i <= n; i++)
        cin >> w[i];
    for (int i = 1; i <= n; i++)
        cin >> r[i];

    games.resize(size);
    for (int i = 1; i <= n; i++)
        games[i].assign(size, 0);

    int summ_flow = 0;
    for (int i = 1; i <= n; i++) {
        for (int j = 1; j <= n; j++) {
            int a;
            cin >> a;
            if (a > 0 && games[i][j] == 0) {
                add_edge(s, t, a);
                summ_flow += a;
                add_edge(t, i, INF);
                add_edge(t, j, INF);
                games[i][j] = a;
                games[j][i] = a;
                t++;
            }
        }
    }

    add_edge(1, t, INF);
    w[1] += r[1];
    for (int i = 2; i <= n; i++) {
        int cnt = w[1] - w[i];
        if (cnt > 0)
            add_edge(i, t, cnt);
    }
    n = t + 1;

    if (*(max_element(w.begin(), w.end())) != w[1]) {
        cout << "NO";
    } else if (algorithmDinica() >= summ_flow) {
        cout << "YES";
    } else {
        cout << "NO";
    }

    cout << endl;
    return 0;
}