#include "tbb/parallel_sort.h"
#include <cmath>
#include <iostream>
#include <vector>
#include <boost/timer/timer.hpp>

const size_t N = 100000000;

using std::cout;
using std::endl;

int main()
{
    cout << "init... " << endl;
    double *a = new double[N];
    for (size_t i = 0; i < N; i++) {
        a[i] = sin(i);
    }
    cout << "sorting... " << endl;
    
    boost::timer::cpu_timer timer;
    
    tbb::parallel_sort(a, a + N);
    // std::sort(a, a + N);
    
    boost::timer::cpu_times elapsed_times(timer.elapsed());
    cout << "sorting take: " << format(elapsed_times, 9) << endl;
    
    delete[] a;

    return 0;
}