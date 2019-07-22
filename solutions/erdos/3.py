def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

class Fraction:
    def __init__(self, a, b):
        g = gcd(a, b)
        self.a = a // g
        self.b = b // g
    
    def __add__(self, F):
        return Fraction(self.a * F.b + F.a * self.b, self.b * F.b)
    
    def reciprocal(self):
        self.a, self.b = self.b, self.a
        return self
    
    def __str__(self):
        return "{}/{}".format(self.a, self.b)

class Irrational:
    def __init__(self, a, b, c, n):
        g = gcd(a, gcd(b, c))
        self.a = a // g
        self.b = b // g
        self.c = c // g
        self.n = n
    
    def reciprocal(self):
        if self.a * self.a > self.n * self.b * self.b:
            return Irrational(self.c * self.a, -self.c * self.b, self.a * self.a - self.n * self.b * self.b, self.n)
        else:
            return Irrational(-self.c * self.a, self.c * self.b, self.n * self.b * self.b - self.a * self.a, self.n)
        
    
    def helper(self):
        l = 0
        r = self.a + self.n
        
        while r - l != 1:
            m = l + r >> 1
            if self.b * self.b * self.n >= (m * self.c - self.a) ** 2:
                l = m
            else:
                r = m
        
        return Fraction(l, 1), Irrational(self.a - l * self.c, self.b, self.c, self.n).reciprocal()

class Continued:
    def __init__(self, a, b):
        self.a = a
        self.lst = [b]
        first = (b.a, b.b, b.c)
        
        while True:
            a, b = self.lst.pop().helper()
            self.lst += [a, b]
            
            tmp = (b.a, b.b, b.c)
            if tmp == first:
                self.lst.pop()
                self.n = len(self.lst)
                break
    
    def helper(self):
        frac = Fraction(0, 1)
        for i in reversed(range(self.n - 1)):
            frac += self.lst[i]
            frac.reciprocal()
        return frac
    
    def rationalize(self, frac):
        for i in reversed(range(self.n)):
            frac += self.lst[i]
            frac.reciprocal()
        return frac

def main():
    def solve(n):
        cf = Continued(*Irrational(0, 1, 1, n).helper())
        frac = cf.helper()
        
        while True:
            tmp = cf.a + frac
            x = tmp.a
            y = tmp.b
            if x ** 2 == n * y ** 2 + 1:
                return x
            
            frac = cf.rationalize(frac)
    
    ma = 0
    ans = 0
    j = 1
    for i in range(1, int(input()) + 1):
        if j * j == i:
            j += 1
            continue
        
        tmp = solve(i)
        if tmp > ma:
            ma = tmp
            ans = i
            print(ans, ma)
    
    print(ans)
    
    return 0

main()
