--Given a number, get its last digi
-- is 123 -> 3
lastDigit :: Integer -> Integer
lastDigit x = mod x 10

--Given a number, drop every digit except the last one
dropLastDigit :: Integer -> Integer
dropLastDigit x = div y 10
                  where y = x - lastDigit x


toRevDigits :: Integer -> [Integer]
toRevDigits x
  | x <= 0 = []
  | otherwise = (lastDigit x):toRevDigits (dropLastDigit x)

--write a function that doubles every other Integer in a list.
--note that this should take a list of digits that are already in reverse order.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:rest) = x:(2*y):(doubleEveryOther rest)

--Define a function that calculates the sum of all digits in a list of integers
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumDigit x + sumDigits xs

sumDigit :: Integer -> Integer
sumDigit 0 = 0
sumDigit x = lastDigit x + sumDigit (dropLastDigit x)

--Define a function that indicates whether an Integer could be a valid credit card number
luhn :: Integer -> Bool
luhn x = if mod (sumDigits (doubleEveryOther(toRevDigits x))) 10 == 0
         then True
         else False
