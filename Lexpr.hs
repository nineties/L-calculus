import Data.List (elemIndices)

type Variable = String

-- Law Lambda Representation
data RawTerm = Var Variable
             | App RawTerm RawTerm
             | Lam Variable RawTerm
             deriving (Show)

-- Thread and Pair representation
data ThreadR = Thread [Variable] Variable
             | Pair [Variable] ThreadR ThreadR
             deriving (Show)

-- Combinator Representation
data CombR = T Int Int
           | P Int CombR CombR
           deriving (Show)

to_threads :: RawTerm -> ThreadR
to_threads(Var x) = Thread [] x
to_threads(App t1 t2) = Pair [] (to_threads t1) (to_threads t2)
to_threads(Lam x t) = case to_threads t of
                        Thread xs t -> Thread (x:xs) t
                        Pair xs t1 t2 -> Pair (x:xs) t1 t2

to_comb :: ThreadR -> CombR
to_comb(Thread xs x) = T (length xs) (last $ elemIndices x xs)
to_comb(Pair xs t1 t2) = P (length xs) (to_comb (app xs t1)) (to_comb (app xs t2))
    where
    app xs (Thread ys x) = Thread (xs ++ ys) x
    app xs (Pair ys t1 t2) = Pair (xs ++ ys) t1 t2

reduce :: CombR -> CombR
reduce(T k i) = T k i
reduce(P n (T k i) a) 
    | i < n     = T (k-1) i
    | i == n    = lift (k-n-1) n a
    | otherwise = T (k-1) (i-1)
    where
    lift m n (T k i)
        | i < k && n <= i   = T (k+m) (i+n)
        | otherwise         = T (k+m) i
    lift m n (P k a b) = P (k+m) (lift m n a) (lift m n b)
reduce(P n (P k a b) p) = reduce $ P (k-1) (reduce $ P n a p) (reduce $ P n b p)

test_expr = Lam "t" (App (App (Lam "f" (Lam "x" (App (Var "f") (Var "x")))) (Lam "x" (Var "x"))) (Var "t"))

main = do
    print $ test_expr
    print $ to_threads test_expr
    print $ to_comb . to_threads $ test_expr
    print $ reduce . to_comb . to_threads $ test_expr
