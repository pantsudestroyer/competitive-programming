----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----    -----------------    -----------------    -----------------    ----------------------------
----    -----------------    -----------------    -----------------    ----------------------------
----    -----------------    -----------------    -----------------    ----------------------------
----    -----------------    -----------------    -----------------    ----------------------------
----    -----------------    -----------------    -----------------    ----------------------------
----    -----------------    -----------------    -----------------    ----------------------------
----    -----------------    -----------------    -----------------    ----------------------------
----    -----------------    -----------------    -----------------    ----------------------------
----    -----    --------    -----    --------        -------------        ------------------------
----    ----    ---------    ----    ---------    -    ------------    -    -----------------------
----    ---    ----------    ---    ----------    --    -----------    --    ----------------------
----    --    -----------    --    -----------    ---    ----------    ---    ---------------------
----    -    ------------    -    ------------    ----    ---------    ----    --------------------
----        -------------        -------------    -----    --------    -----    -------------------
----        -------------        -------------    -----    --------    -----    -------------------
----    -    ------------    -    ------------    ----    ---------    ----    --------------------
----    --    -----------    --    -----------    ---    ----------    ---    ---------------------
----    ---    ----------    ---    ----------    --    -----------    --    ----------------------
----    ----    ---------    ----    ---------    -    ------------    -    -----------------------
----    -----    --------    -----    --------        -------------        ------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

--Input/Output/String Parsing

stringSplit :: String -> String -> [String]
stringSplit to from
    | from == ""        = [to]
    | head from == ' '  = (if to == "" then [] else [to]) ++ (stringSplit "" $ tail from)
    | otherwise         = stringSplit (to ++ [head from]) $ tail from

printListChar :: [Char] -> IO ()
printListChar xxs
    | null xxs = do
        putStr "\n"
    | otherwise = do
        _printListChar xxs
        putStr "\n"

_printListChar :: [Char] -> IO ()
_printListChar xxs
    | null xs = do
        putStr [x]
    | otherwise = do
        putStr [x]
        putStr " "
        _printListChar xs
    where
        (x : xs) = xxs

printListString :: [String] -> IO ()
printListString xxs
    | null xxs = do
        putStr "\n"
    | otherwise = do
        _printListString xxs
        putStr "\n"

_printListString :: [String] -> IO ()
_printListString xxs
    | null xs = do
        putStr x
    | otherwise = do
        putStr x
        putStr " "
        _printListString xs
    where
        (x : xs) = xxs

printListOther :: (Show a) => [a] -> IO ()
printListOther xxs
    | null xxs = do
        putStr "\n"
    | otherwise = do
        _printListOther xxs
        putStr "\n"

_printListOther :: (Show a) => [a] -> IO ()
_printListOther xxs
    | null xs = do
        putStr $ show x
    | otherwise = do
        putStr $ show x
        putStr " "
        _printListOther xs
    where
        (x : xs) = xxs

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

--Math

myGcd x y
    | y == 0    = x
    | otherwise = myGcd y $ x `mod` y

eGcd x y
    | y == 0    = (1, 0)
    | otherwise = let (a, b) = eGcd y (x - k * y) in (b, a - k * b)
    where
        k = x `div` y

powMod base exponent modulo
    | exponent == 0         = if modulo == 1 then 0 else 1
    | exponent `mod` 2 == 0 = powMod base (exponent `div` 2) modulo ^ 2 `mod` modulo
    | otherwise             = base * powMod base (exponent - 1) modulo `mod` modulo

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

--Data Structures

--------------------------------------------------

data Array = EmptyArray | ArrayNode {index :: Int, arrayValue :: Int, leftArray :: Array, rightArray :: Array} deriving (Show, Read, Eq, Ord)

newArray :: Int -> Int -> Array
newArray l r
    | r - l == 0 = ArrayNode l 0 EmptyArray EmptyArray
    | r - l == 1 = ArrayNode r 0 (newArray l l) EmptyArray
    | otherwise  = ArrayNode m 0 (newArray l (m - 1)) (newArray (m + 1) r)
    where
        m = (l + r) `div` 2

valueOf :: Array -> Int -> Int
valueOf (ArrayNode index value leftArray rightArray) ind
    | index == ind = value
    | index <  ind = valueOf rightArray ind
    | index >  ind = valueOf leftArray  ind

replace :: Array -> Int -> Int -> Array
replace (ArrayNode index value leftArray rightArray) ind val
    | index == ind = ArrayNode index val   leftArray rightArray
    | index <  ind = ArrayNode index value leftArray (replace rightArray ind val)
    | index >  ind = ArrayNode index value (replace leftArray ind val) rightArray

printArray :: Array -> IO ()
printArray array
    | otherwise = do
        _printArray array
        putStr "\n"

_printArray :: Array -> IO ()
_printArray EmptyArray
    | otherwise = do
        return ()
_printArray (ArrayNode index value leftArray rightArray)
    | otherwise = do
        _printArray leftArray
        putStr $ show value
        putStr " "
        _printArray rightArray

turnListToArray :: [Int] -> Array
turnListToArray xxs
    | otherwise = _turnListToArray (newArray 0 (len + 1)) xxs 1 len
    where
        len = length xxs 

_turnListToArray :: Array -> [Int] -> Int -> Int -> Array
_turnListToArray array xxs l r
    | l > r     = array
    | otherwise = _turnListToArray (replace array l x) xs (l + 1) r
    where
        (x : xs) = xxs

--------------------------------------------------

data Stack = EmptyStack | StackNode {front :: Int, stackTail :: Stack} deriving (Show, Read, Eq, Ord)

newStack = EmptyStack

putOnStack :: Stack -> Int -> Stack
putOnStack stack val
    | otherwise = StackNode val stack

valueOfTop :: Stack -> Int
valueOfTop (StackNode front stackTail)
    | otherwise = front

popStack :: Stack -> Stack
popStack (StackNode front stackTail)
    | otherwise = stackTail

--------------------------------------------------

data Queue = QueueNode {inStack :: Stack, outStack :: Stack} deriving (Show, Read, Eq, Ord)

newQueue = QueueNode EmptyStack EmptyStack

putInQueue :: Queue -> Int -> Queue
putInQueue (QueueNode inStack outStack) val
    | otherwise = QueueNode (putOnStack inStack val) outStack

_transferStack :: Stack -> Stack -> Stack
_transferStack inStack outStack
    | inStack == EmptyStack = outStack
    | otherwise             = _transferStack (popStack inStack) (putOnStack outStack (valueOfTop inStack))

updatedQueue :: Queue -> Queue
updatedQueue (QueueNode inStack outStack)
    | otherwise = QueueNode EmptyStack (_transferStack inStack outStack)

valueOfFront :: Queue -> Int -> Int
valueOfFront (QueueNode inStack outStack) val
    | otherwise = (valueOfTop outStack)

popQueue :: Queue -> Queue
popQueue (QueueNode inStack outStack)
    | otherwise = QueueNode inStack (popStack outStack)

--------------------------------------------------

data Segtree = EmptySegtree | SegtreeNode {left :: Int, right :: Int, segtreeValue :: Int, leftSegtree :: Segtree, rightSegtree :: Segtree} deriving (Show, Read, Eq, Ord)

newSegtree :: Int -> Int -> Segtree
newSegtree l r
    | r - l == 1    = SegtreeNode l r 0 EmptySegtree EmptySegtree
    | otherwise     = SegtreeNode l r 0 (newSegtree l m) (newSegtree m r)
    where
        m = (l + r) `div` 2

query :: Segtree -> Int -> Int -> Int
query (SegtreeNode left right value leftSegtree rightSegtree) l r
    | l <= left && right <= r   = value
    | r <= left || right <= l   = 0
    | otherwise                 = (query leftSegtree l r) + (query rightSegtree l r)

update :: Segtree -> Int -> Int -> Segtree
update (SegtreeNode left right value leftSegtree rightSegtree) ind val
    | right - left == 1         = SegtreeNode left right val EmptySegtree EmptySegtree
    | m <= ind                  = let newNode = update rightSegtree ind val in SegtreeNode left right (_update leftSegtree newNode)  leftSegtree newNode
    | otherwise                 = let newNode = update leftSegtree  ind val in SegtreeNode left right (_update newNode rightSegtree) newNode     rightSegtree
    where
        m = (left + right) `div` 2

_update :: Segtree -> Segtree -> Int
_update seg tree
    | otherwise = (segtreeValue seg) + (segtreeValue tree)

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

solve t
    | t == 0 = do
        return ()
    | otherwise = do
        _solve
        putStr "\n"
        solve (t - 1)

_solve = do
    input <- getLine
    return ()

main = do
    t <- getLine
    solve (read t)
    return ()
