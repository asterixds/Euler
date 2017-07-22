"""Problem 1: Find the sum of all the multiples of 3 or 5 below 1000"""
print sum( x for x in range(1000) if  ((x % 3 == 0 )or ( x % 5 == 0)))


"""Problem 2: Find the sum of all the even-valued terms in the sequence which 
do not exceed four million"""
def fibonacci():
    x,y = 0,1
    while True:
        yield x
        x,y = y,x+y    

def even(seq):
    for n in seq:
        if (n % 2 == 0):
            yield n

def less_than_4_mil(seq):
    for n in seq:
        if n > 4000000:
            break
        yield n

print (sum(less_than_4_mil(even(fibonacci()))))

"""Problem 3:  Find the largest prime factor of 317584931803"""

def isPrime(n):
    if n > 2:
        for i in range(2, int(n**0.5) + 1):
            if n % i == 0:
                return False
        return True

def prime_factors(n):
    i = 2
    factors = []
    while i * i <= n:
        if n % i:
            i = i + 1
        else:
            n = n // i
            factors.append(i)
    if n > 1:
        factors.append(n)
    return factors

num =600851475143
f = max(prime_factors(num))
print("largest prime factor of {0} is {1}".format(num,f))


"""Problem 4: A palindromic number reads the same both ways
The largest palindrome made from the product of two 2-digit numbers is 9009 
Find the largest palindrome made from the product of two 3-digit numbers"""

def isPalindrome1(n):
    num = n
    rev = 0
    while (n > 0):
      dig = n % 10
      rev = rev * 10 + dig
      n = n / 10
    return rev == num

def isPalindrome(n):
    n_str = str(n)
    return n_str ==n_str[::-1]

unique = list(set([x*y for x in range(100,999) for y in range(100,999)]))
top =  max(filter(lambda n: isPalindrome(n),unique))
print top


"""Problem 5: What is the smallest number divisible by each of the numbers 1 to 10?"""
def gcd(a,b): return b and gcd(b, a % b) or a
def lcm(a,b): return a * b / gcd(a,b)
n = 1
for i in range(1, 10):
    n = lcm(n, i)
print n

"""Problem 6: What is the difference between the sum of the squares and the square of the sums?"""
numbers = range(1,100)
def square(n):
    return n * n
print square(sum(numbers)) - sum(map(lambda x: square(x),numbers))

