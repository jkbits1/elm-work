import Text(..)

fibo : Int -> Int
fibo a = 
--  sqrt((a^2) + (b^2))
  case a of
    0 -> 0
    1 -> 1
    a -> fibo (a-1) + fibo (a-2)
  
--  f00
--  f11
--  fn fn-1 plus fn-2

--main = asText [fibo 3, fibo 3, fibo 3]
main = asText [fibo 0, fibo 1, fibo 2, fibo 3, fibo 4, fibo 5, fibo 6]

