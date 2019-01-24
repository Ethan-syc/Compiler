fun min (a:int, b:int):int =
    if (a < b) then a else b
fun fib (x:int):int =
    if x <= 0 then 1 else
    let fun fib_tail (1, result1, result) = result+result1
          | fib_tail (x, result1, result) = fib_tail (x-1, result, result+result1)
    in fib_tail (x,0,1)
    end
fun isPrime (x:int):bool =
    if x <= 1 then false else
    let fun isPrime_helper (y,x) = 
            if y*y > x then true else
            if x mod y = 0 then false else
            isPrime_helper (y+1,x)
    in isPrime_helper (2, x)
    end
fun sumList [] = 0
  | sumList [x] = x
  | sumList (a::l) = a + sumList l
fun squareList [] = []
  | squareList [(x:int)] = [x*x]
  | squareList (a::l) = a*a::squareList l
