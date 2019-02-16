-- fibTopDownRec
fibTopDownRec :: Int -> Int
fibTopDownRec 1 = 1
fibTopDownRec 2 = 1
fibTopDownRec n = fibTopDownRec (n - 1) + fibTopDownRec(n - 2)
--

-- main execution of program
main :: IO()
main = 
    print(fibTopDownRec 6)
--