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
    boost::timer::cpu_timer timer;
    cout << "init... " << endl;
    double *a = new double[N];
    for (size_t i = 0; i < N; i++) {
        a[i] = sin(i);
    }
    
    cout << "sorting take: " << format(boost::timer::cpu_times(timer.elapsed()), 9) << endl;
    timer.start();
    cout << "sorting... " << endl;    
    //tbb::parallel_sort(a, a + N);
    std::sort(a, a + N);
    
    cout << "sorting take: " << format(boost::timer::cpu_times(timer.elapsed()), 9) << endl;
    
    delete[] a;

    return 0;
}