-- data type
data MemberType = IntType Int | StringType String deriving (Ord, Eq, Show)  

type Collection = [MemberType]
instance Num MemberType where 
    (IntType a) + (IntType b) = IntType (a + b)

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
fibTopDownIter

--

-- main execution of program
main :: IO()
main = 
    print(fibTopDownRec 6) >>
    print(fibBottomUpIter 6) >>
    print(head arr + head (tail arr))
--