#include <iostream>
#include <vector>
#include <limits>
#include <utility>
#include <algorithm>
#include <string>
#include <complex>

using namespace std;

typedef complex<double> base;
typedef vector<int> vi;
const double PI = 3.1415926535;

void fft(vector<base> &a, bool invert)
{
    int n = (int)a.size();
    if (n == 1)
        return;

    vector<base> a0(n / 2), a1(n / 2);
    for (int i = 0, j = 0; i < n; i += 2, ++j) {
        a0[j] = a[i];
        a1[j] = a[i + 1];
    }
    fft(a0, invert);
    fft(a1, invert);

    double ang = 2 * PI / n * (invert ? -1 : 1);
    base w(1), wn(cos(ang), sin(ang));
    for (int i = 0; i < n / 2; ++i) {
        a[i] = a0[i] + w * a1[i];
        a[i + n / 2] = a0[i] - w * a1[i];
        if (invert)
            a[i] /= 2, a[i + n / 2] /= 2;
        w *= wn;
    }
}

void multiply(vector<int> const &a, vector<int> const &b, vector<int> &res)
{
    vector<base> fa(a.begin(), a.end()), fb(b.begin(), b.end());
    size_t n = 1;
    while (n < max(a.size(), b.size()))
        n <<= 1;
    n <<= 1;
    fa.resize(n), fb.resize(n);

    fft(fa, false), fft(fb, false);
    for (size_t i = 0; i < n; ++i)
        fa[i] *= fb[i];
    fft(fa, true);

    res.resize(n);
    for (size_t i = 0; i < n; ++i)
        res[i] = int(fa[i].real() + 0.5);
    int carry = 0;
    for (size_t i = 0; i < n; ++i) {
        res[i] += carry;
        carry = res[i] / 10;
        res[i] %= 10;
    }
}

void read(vector<int> &a)
{
    a.clear();
    string s;
    cin >> s;
    for (int i = s.length() - 1; i >= 0; --i) {
        a.push_back(s[i] - '0');
    }
}

int main()
{
    ios_base::sync_with_stdio(0);
    int n;
    cin >> n;
    vi a, b, res;
    for (int i = 0; i < n; ++i) {
        read(a);
        read(b);
        res.clear();
        multiply(a, b, res);
        int j = res.size() - 1;
        while (res[j] == 0 && j >= 0)
            --j;
        if (j < 0)
            cout << 0;
        else {
        }
        for (; j >= 0; --j) {
            cout << res[j];
        }
        cout << endl;
    }
    return 0;
}