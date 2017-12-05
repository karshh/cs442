data Primitive = Succ | IsZero

data Term = Var String | Abs String Term | App Term Term
          | Prim Primitive | INT Int

instance Show Term where
	show (Var x) = x
	show (Abs x e) = "(\\" ++ x ++ "." ++ (show e) ++ ")"
	show (App a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
	show (INT i) = (show i)
	show (Prim Succ) = "[[Succ]]"
	show (Prim IsZero) = "[[IsZero]]"

tRUE = Abs "x" (Abs "y" (Var "x"))
fALSE = Abs "x" (Abs "y" (Var "y"))

applyPrim :: Primitive -> Term -> Term
applyPrim Succ (INT i) = INT(i + 1)
applyPrim IsZero (INT i) = if i == 0 then tRUE else fALSE

data SContents = S Term | Closure(String, Term, [EContents])   
data EContents = E (String, SContents)
data CContents = C Term | Apply
data DContents = D ([SContents], [EContents], [CContents])

data SECDConfig = SECD ([SContents], [EContents], [CContents], [DContents]) | Return SContents

lookUp :: String -> [EContents] -> SContents
lookUp x (E(a,b):ys) = if x == a then b else lookUp x ys
 
secdOneStep :: SECDConfig -> SECDConfig
secdOneStep (Return m) = Return m
secdOneStep (SECD(a, b, C(Var x):c, d)) = SECD((lookUp x b):a, b, c, d)
secdOneStep (SECD(a, b, C(Abs x m):c, d)) = SECD(Closure(x, m, b):a, b, c, d)
secdOneStep (SECD(a, b, C(App m n):c, d)) = SECD(a, b, C(n):C(m):Apply:c, d)
secdOneStep (SECD(a, b, C(Prim p):c, d)) = SECD(S(Prim p):a, b, c, d)
secdOneStep (SECD(S(Prim p):S(n):a, b, Apply:c, d)) = SECD(a, b, C(applyPrim p n):c, d)
secdOneStep (SECD(Closure(x, m, e):n:a, b, Apply:c, d)) = SECD([], E(x,n):b, [C(m)], D(a,b,c):d)
secdOneStep (SECD(m:[], b, [], D(x,y,z):d)) = SECD(m:x, y, z, d)
secdOneStep (SECD(m:[], _, [], [])) = Return m

closureTrans :: SContents -> Term
closureTrans (S x) = x
closureTrans (Closure(x, m, env)) = Abs x (subst m env) where
	subst a [] = a
	subst (App a b) env = App (subst a env) (subst b env)
	subst (Var x) (E(a,b):xs) = if x == a then closureTrans b else Var x
	subst a _ = a

instance Show SContents where
	show (S(a)) = show a
	show (Closure(a,b,c)) = "(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"

instance Show EContents where
	show (E(x,y)) = "(" ++ x ++ "," ++ show y ++ ")"

instance Show CContents where
	show (C(x)) = show x
	show Apply = "@"

instance Show DContents where
	show (D(a,b,c)) = "(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"

instance Show SECDConfig where
	show (Return m) = show m ++ "\n"
	show (SECD(a,b,c,d)) =
	 	"S = " ++ show a ++ "\n" ++
	 	"E = " ++ show b ++ "\n" ++
	 	"C = " ++ show c ++ "\n" ++
	 	"D = " ++ show d ++ "\n" 
    
reduce  :: Term -> Term
reduce x = sECDmachine(SECD([], [], [C(x)], [])) where
	sECDmachine(Return m) = closureTrans m
	sECDmachine(x) = sECDmachine(secdOneStep(x)) 

 
