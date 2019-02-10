#include<bits/stdc++.h>
using namespace std;

int arr[1001], MOD;

struct mat2
{
    long long ul; long long ur;
    long long ll; long long lr;
    
    mat2& mult(mat2 B)
    {
        mat2 A;
        swap(*this, A);
        
        *this = {(A.ul * B.ul + A.ur * B.ll) % MOD, (A.ul * B.ur + A.ur * B.lr) % MOD,
                 (A.ll * B.ul + A.lr * B.ll) % MOD, (A.ll * B.ur + A.lr * B.lr) % MOD};
        
        return *this;
    }
    
    mat2& mpow(int n)
    {
        mat2 A = {1, 0, 0, 1};
        swap (*this, A);
        
        while (n)
        {
            if (n % 2)
                (*this).mult(A);
            A.mult(A);
            n >>= 1;
        }
        
        return *this;
    }
};

int modpow(unsigned long long b, unsigned long long e, int n)
{
    long long a = 1;
    b %= n;
    
    while (e)
    {
        if (e % 2)
            a = (a * b) % n;
        b = (b * b) % n;
        e >>= 1;
    }
    
    return a % n;
}

void init()
{
    arr[1] = 1;
    for (int i = 2; i != 1001; ++i)
    {
        int a = 1, b = 1, c = 2, d;
        while ((a - 1) || b)
        {
            ++c;
            d = a;
            a = b;
            b = (b + d) % i;
        }
        arr[i] = c;
    }
}

mat2 fib = {1, 1, 1, 0};
mat2 inv = {0, 1, 1, -1};

int main()
{
    init();
    unsigned long long a, b, n, t;
    for (cin >> t; t--;)
    {
        cin >> a >> b >> n;
        MOD = n;
        a = modpow(a, b, arr[n]);
        mat2 A = fib;
        cout << A.mpow(a).mult(inv).ul << endl;
    }
    return 0;
}
