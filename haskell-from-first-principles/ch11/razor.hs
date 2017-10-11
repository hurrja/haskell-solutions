data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add a b) = (eval a) + (eval b)

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add a b) = (printExpr a) ++ " + " ++ (printExpr b)

a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2
