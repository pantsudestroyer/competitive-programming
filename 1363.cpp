#include<bits/stdc++.h>
using namespace std;

int main()
{
    long long n, k;
    while (scanf("%lld %lld\n", &n, &k) != EOF)
    {
        long long r;
        if (n < k)
        {
            int i = (k + n - 1) / n;
            r = ((n - k / i) * (2 * (k % (i - 1)) + (i - 1) * (2 * (k / (i - 1)) - k / i - n - 1))) >> 1;
            while (k / i)
            {
                i = k / (k / i);
                r += ((k / i - k / (i + 1)) * (2 * (k % i) + i * (k / i - k / (i + 1) - 1))) >> 1;
                ++i;
            }
        }
        else
        {
            int i = 1;
            r = k * (n - k);
            while (k / i)
            {
                i = k / (k / i);
                r += ((k / i - k / (i + 1)) * (2 * (k % i) + i * (k / i - k / (i + 1) - 1))) >> 1;
                ++i;
            }
        }
        printf("%lld\n", r);
    }
    return 0;
}