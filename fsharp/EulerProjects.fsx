//Problem 1: Find the sum of all the multiples of 3 or 5 below 1000
#time
let problem1 = [1..999] |> List.map (fun i -> if i % 5 = 0 || i % 3 = 0 then i else 0) |> List.sum
#time



//Problem 2
//Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million.
open System.Numerics
 
let fibonacci =
    Seq.unfold
        (fun (c, n) -> Some(c, (n, c + n)))
        (BigInteger 0, BigInteger 1)
#time
let problem2 = 
    fibonacci
        |> Seq.takeWhile(fun n -> n < BigInteger 4000000 ) 
        |> Seq.filter(fun n -> n % BigInteger 2 = BigInteger 0)
        |> Seq.sum
#time



//Problem 3: Find the largest prime factor of a composite number
let primeFactors = 
    let rec recPrimeFactors primes i n =
            match n with
                | n when 2L*i > n ->  n::primes //terminate
                | n -> match n % i with
                        | 0L -> recPrimeFactors  (i::primes) 2L (n/i)  //restart after factorisation
                        |_ ->  recPrimeFactors primes (i+ 1L) n //increment divisor
    recPrimeFactors  [] 2L

#time
let problem3 = primeFactors 600851475143L |> Seq.head
#time



//Problem 4
//Find the largest palindrome made from the product of two 3-digit numbers.
open System.Linq
let isPalindrome n =
    let s1 = n.ToString().ToCharArray()
    let rs1 = Array.rev s1
    s1.SequenceEqual(rs1)

#time
let problem4 = [100..999]
            |> Seq.collect(fun i -> [i..999] |> Seq.map(fun y -> y*i))
            |> Seq.filter(isPalindrome)
            |> Seq.max
#time

//Problem 5: What is the smallest number divisible by each of the numbers 1 to 20?
let rec gcd a b = 
        if b = 0 then a
        else gcd b (a % b)

let lcm a b =
        b * (a / (gcd a b))
#time
let  problem5= [1..20] |> List.fold (fun acc x -> lcm acc x) 1 
#time



//Problem 6: What is the difference between the sum of the squares and the square of the sums?
let square n = 
    n * n

let l = [1..100]
let f = square
#time
let problem6 = (l |> Seq.sum |> f) - (l |> Seq.map(fun x -> f x) |> Seq.sum)   
#time



//Problem 7: Find the 10001st prime
//Slow method
let factorsOf n =
    let limit = n |> double |> sqrt |>int64
    [2L..limit] |> Seq.filter(fun x -> n % x = 0L)

let isPrime n  = factorsOf n |> Seq.length = 0
let primes = Seq.unfold (fun (c:int64) -> Some(c, c+1L)) 2L |> Seq.filter isPrime
#time
let tenkPrime = primes |> Seq.item(10000)
#time

//Fast method
let numbers = Seq.unfold (fun (c:int64) -> Some(c, c+1L)) 2L
let primes = numbers |> Seq.filter( fun i -> numbers |> Seq.takeWhile(fun j -> j*j <=i) |> Seq.forall(fun z -> i % z >0L ) )
#time
let problem9 = primes |> Seq.item(10000)
#time



//Problem 8: Discover the largest product of five consecutive digits in the 1000-digit number.
let s = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
let prod w = w |> Seq.fold(fun acc x -> acc * x) 1 

#time
let problem8 = s |> Seq.map (fun c -> int32(c.ToString())) |> Seq.windowed(5) |> Seq.map(fun x -> prod x) |> Seq.max 
#time




//Problem 9: Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.
let pythagoreanTriplets ubound = 
    let hyp a b = ubound - a - b 
    seq {
        for a in [1..ubound] do
            for b in [a..ubound] do
                let c = hyp a b
                if a+b+ c = ubound && a*a+b*b=c*c then
                    yield a*b*c
    }

#time
let problem9 = pythagoreanTriplets 1000 |> Seq.head
#time



//Problem 10: Calculate the sum of all the primes below two million
let numbers = Seq.unfold (fun (c:int64) -> if (c >= 2000000L) then None else Some(c, c+1L)) 2L
let primes = numbers |> Seq.filter( fun i -> numbers |> Seq.takeWhile(fun j -> j*j <=i) |> Seq.forall(fun z -> i % z >0L ) )
#time
let problem9 = primes |> Seq.reduce (fun x y -> x +  y)
#time



//Problem 11: What is the greatest product of four numbers on the same straight line in the 20 by 20 grid?
let s ="08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08\n\
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00\n\
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65\n\
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91\n\
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80\n\
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50\n\
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70\n\
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21\n\
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72\n\
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95\n\
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92\n\
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57\n\
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58\n\
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40\n\
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66\n\
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69\n\
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36\n\
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16\n\
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54\n\
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"


let sequence s = 
    s.Split('\\n')
    |> Seq.map (Split(' ') )
    |> Seq.map (Seq.map (Int32.Parse))

