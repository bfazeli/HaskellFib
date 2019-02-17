import Debug.Trace

-- data type
data MemberType = IntType Int | StringType String deriving (Ord, Eq, Show)

type Collection = [MemberType]
instance Num MemberType where 
    (IntType a) + (IntType b) = IntType (a + b)
    (IntType a) - (IntType b) = IntType (a - b)

-- arr::Collection
-- arr = [IntType 1, IntType 2, StringType "Herro", IntType 5]
--


-- while loop
while:: state -> (state -> Bool) -> (state -> state) -> (state -> result) -> result
while state shouldContinue action finalAction =
    if shouldContinue state then while (action state) shouldContinue action finalAction
    else finalAction state
--

-- fibTopDownRec
fibTopDownRec :: Int -> Int
fibTopDownRec 1 = 1
fibTopDownRec 2 = 1
fibTopDownRec n = 
    fibTopDownRec (n - 1) + fibTopDownRec(n - 2)
--

-- fibBottomUpIter
fibBottomUpIter:: Int -> Int
fibBottomUpIter n =
    while 
        (1, 1, 0) 
        (\(c, _, _) -> c < n) 
        (\(c, ka, kb) -> (c + 1, ka + kb, ka))
        (\(c, ka, kb) -> ka)
--

-- fibTopDownIter
fibAction:: ([MemberType], [MemberType]) -> ([MemberType], [MemberType])
fibAction state
    | (head (fst state) /= StringType "fib") && (head (fst state) /= StringType "+") =
        -- trace("The value of state is: " ++ show state)   -- Toggle the comment on this line to trace
        (tail (fst state), head (fst state) : snd state)
    | head (fst state) == StringType "fib" =
        if head (snd state) <= IntType 2 then (IntType 1 : tail (fst state), tail(snd state))
        else ([ head (snd state) - IntType 2, StringType "fib", head (snd state) - IntType 1, StringType "fib", StringType "+" ] ++ tail (fst state), tail(snd state))
    | otherwise =
        ((head (snd state) + head (tail(snd state))) : tail (fst state), tail(tail(snd state)))

fibTopDownIter:: Int -> Int
fibTopDownIter n =
    while
        ([IntType n, StringType "fib"], [])
        (not . (null . fst))
        fibAction
        (head . snd)
--

-- main execution of program
main :: IO()
main = 
    print(fibTopDownRec 6) >>
    print(fibBottomUpIter 6) >>
    print(fibTopDownIter 4)
--

