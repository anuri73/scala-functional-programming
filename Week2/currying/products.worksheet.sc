def product(f: Int => Int)(a: Int, b: Int):Int = {
    if (a>b) then 1 else f(a) * product(f)(a+1, b)
}

product(x=>x*x)(1, 5)

def factorial(n:Int) = product(x=>x)(1, n)

factorial(5)

def mapReduce(f:Int=>Int, combine:(Int, Int)=>Int, finalValue:Int)(a:Int, b:Int):Int = {
    def recur(a:Int):Int = {
        if (a>b) then finalValue
        else combine(f(a), recur(a+1))
    }
    recur(a)
}

def abstractFactorial(n:Int) = mapReduce(x=>x, (x, y)=>x*y, 1)(1, n)

abstractFactorial(5)

def sum(f:Int => Int) = mapReduce(f, (x, y) => x + y, 0)

sum(factorial)(1, 5)

product(identity)(1,6)