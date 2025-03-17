import Data.List
import Data.Char
import Data.Bits (shiftL, shiftR) -- Got this from https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-Bits.html#v:shiftR
import Numeric (showIntAtBase, showHex) -- Got this from https://hackage.haskell.org/package/base-4.19.1.0/docs/Numeric.html
-- Variables
type Vars = String
-- added
type FName = String
-- Arithmetic expressions
data AExpr = Var Vars | Const Integer
           | Add AExpr AExpr | Sub AExpr AExpr
           | Mul AExpr AExpr | Div AExpr AExpr
           | Exp AExpr AExpr | Mod AExpr AExpr
           -- added
           | LShift AExpr AExpr| RShift AExpr AExpr
           | Bin Integer | Hex Integer | FCall FName [AExpr] | Sum [AExpr]
  deriving Show
-- Boolean expressions
data BExpr = TT | FF          -- the true and false constants
           | And BExpr BExpr | Or BExpr BExpr | Not BExpr -- boolean operations
           | Eq  AExpr AExpr  -- equality of arithmetic expressions
           | Lt  AExpr AExpr  -- true if the first is less than the second
           | Lte AExpr AExpr  -- true if it's less than or equal to
  deriving Show
-- Instructions
data Instr = Assign Vars AExpr            -- assign X to the value of an expression
           | IfThenElse BExpr Instr Instr -- conditional
           | While BExpr Instr            -- looping construct
           | Do [Instr]                   -- a block of several instructions
           | Nop                          -- the "do nothing" instruction
           | Return AExpr
  deriving Show

-- Environments
type Env = [(Vars,Integer)]
type FEnv = [(FName,([Vars],AExpr))] -- function environment
type VEnv = [(Vars,Integer)]   

data Keywords = IfK | ThenK | ElseK | WhileK | NopK | ReturnK deriving Show
data UOps = NotOp deriving Show
data BOps = AddOp | SubOp | MulOp | DivOp | ModOp | ExpOp
          | AndOp | OrOp  | EqOp  | LtOp  | LteOp | SumOp
          -- added
          | LShiftOp | RShiftOp
  deriving Show
data Token = VSym String | CSym Integer | BSym Bool | NSym String 
           | LPar | RPar | LBra | RBra | Semi
           | UOp UOps | BOp BOps | AssignOp
           | Keyword Keywords
           | Err String
           | PA AExpr | PB BExpr | PI Instr | Block [Instr]
            -- added
           | BinSym Integer | HexSym Integer
           | PList [AExpr] | Comma
           | LBrack | RBrack
  deriving Show

-- read binary
readBin :: String -> Integer
readBin = foldl' (\acc x -> acc * 2 + fromIntegral (digitToInt x)) 0


lexer :: String -> [Token]
lexer "" = []
-- boolean operators. putting these first because /\ must be checked before /
lexer ('!':s)      = UOp NotOp : lexer s
lexer ('/':'\\':s) = BOp AndOp : lexer s
lexer ('\\':'/':s) = BOp OrOp  : lexer s
lexer ('=':'=':s)  = BOp EqOp  : lexer s
-- added 
lexer ('<':'<':s) = BOp LShiftOp : lexer s
lexer ('>':'>':s) = BOp RShiftOp : lexer s

lexer ('<':'=':s)  = BOp LteOp : lexer s
lexer ('<':s)      = BOp LtOp  : lexer s
-- arithmetic operators
lexer ('+':s) = BOp AddOp : lexer s
lexer ('*':s) = BOp MulOp : lexer s
lexer ('-':s) = BOp SubOp : lexer s
lexer ('/':s) = BOp DivOp : lexer s
lexer ('%':s) = BOp ModOp : lexer s
lexer ('^':s) = BOp ExpOp : lexer s

-- punctuation
lexer ('[':xs)     = LBrack : lexer xs
lexer (']':xs)     = RBrack : lexer xs
lexer ('(':s) = LPar : lexer s
lexer (')':s) = RPar : lexer s
lexer ('{':s) = LBra : lexer s
lexer ('}':s) = RBra : lexer s
lexer (';':s) = Semi : lexer s
-- keywords
lexer (':':'=':s)             = AssignOp       : lexer s
lexer ('S':'U':'M':xs) = BOp SumOp : lexer xs
lexer s | take 2 s == "if"    = Keyword IfK    : lexer (drop 2 s)
lexer s | take 4 s == "then"  = Keyword ThenK  : lexer (drop 4 s)
lexer s | take 4 s == "else"  = Keyword ElseK  : lexer (drop 4 s)
lexer s | take 5 s == "while" = Keyword WhileK : lexer (drop 5 s)
lexer s | take 3 s == "nop"   = Keyword NopK   : lexer (drop 3 s)
lexer s | take 6 s == "return" = Keyword ReturnK   : lexer (drop 6 s)
-- hex 
lexer ('0':'x':s) = let (hs, rs) = span isHexDigit s
                     in HexSym (read ("0x" ++ hs)) : lexer rs
-- binary
lexer ('0':'b':s) = let (bs, rs) = span (`elem` "01") s
                     in BinSym (readBin bs) : lexer rs
lexer (',':xs)     = Comma : lexer xs
-- constants and variables
lexer s | take 4 s == "True"  = BSym True  : lexer (drop 4 s)
lexer s | take 5 s == "False" = BSym False : lexer (drop 5 s)
lexer (c:s) | isDigit c = let (ds,rs) = span isDigit s
                          in CSym (read (c:ds)) : lexer rs
lexer (c:s) | isLower c = let (vs,rs) = span isAlphaNum s
                          in VSym (c:vs) : lexer rs
lexer (c:s) | isDigit c = let (ds,rs) = span isDigit s
                          in CSym (read (c:ds)) : lexer rs

lexer (c:s) | isLower c = let (vs,rs) = span isAlphaNum s
                         in VSym (c:vs) : lexer rs
lexer (x:xs) | isUpper x = let (ys,zs) = span isAlphaNum xs in NSym    (x:ys)     : lexer zs
-- other
lexer (c:s) | isSpace c = lexer s
lexer s = error ("Unrecognized token: " ++ s)

-- leftover from GUI attempt
binaryToDecimal :: String -> Int
binaryToDecimal = foldl' (\acc x -> acc * 2 + digitToInt x) 0
  where
    digitToInt '0' = 0
    digitToInt '1' = 1
    digitToInt _   = error "Not a binary string"

decimalToBinary :: Int -> String
decimalToBinary 0 = "0"
decimalToBinary n = reverse $ map intToDigit $ unfoldr (\x -> if x == 0 then Nothing else Just (mod x 2, div x 2)) n

addBinary :: String -> String -> String
addBinary x t = decimalToBinary (binaryToDecimal x + binaryToDecimal t)

subBinary :: String -> String -> String
subBinary x t = decimalToBinary (binaryToDecimal x - binaryToDecimal t)

multBinary :: String -> String -> String
multBinary x t = decimalToBinary (binaryToDecimal x * binaryToDecimal t)

hexToDecimal :: String -> Int
hexToDecimal hex = foldl (\acc x -> acc * 16 + digitToInt x) 0 hex

decimalToHex :: Int -> String
decimalToHex n
    | n == 0 = "0"
    | otherwise = reverse $ unfoldr (\x -> if x == 0 then Nothing else Just (intToDigit $ x `mod` 16, x `div` 16)) n


addHex :: String -> String -> String
addHex x y = decimalToHex (hexToDecimal x + hexToDecimal y)

subHex :: String -> String -> String
subHex x y = decimalToHex (hexToDecimal x - hexToDecimal y)

multHex :: String -> String -> String
multHex x y = decimalToHex (hexToDecimal x * hexToDecimal y)

shiftRight :: String -> Int -> String
shiftRight bi n = drop n bi

shiftLeft :: String -> Int -> String
shiftLeft bi n = reverse (drop n bi)

orOp :: String -> String -> String
orOp x y = zipWith (\x y -> if x == '1' || y == '1' then '1' else '0') x y

andOp :: String -> String -> String
andOp x y = zipWith (\x y -> if x == '1' && y == '1' then '1' else '0') x y


readProg :: [Token] -> [Instr]
readProg ts = case (sr [] (LBra : ts ++ [RBra])) of
  [PI (Do is)] -> is
  s       -> error $ "Parse error: " ++ show s
-- from lecture0320 
--parseLines :: [String] -> [(Vars,AExpr)]
--parseLines = map (parseLine . lexer) 

  
main :: IO ()
main = do
  putStrLn "Enter a file to load"
  filename <- getLine
  input <- readFile filename
  let lexed = lexer input
  let parsed = sr [] lexed
  -- putStrLn $ "The parse of the program is: " ++ show parsed
  let prog = readProg lexed
  putStrLn $ "The parse of the program is: " ++ show prog
  let result = run prog
  putStrLn $ "Answer: " ++ (show result)
  
-- from hw7 

update :: (Vars, Integer) -> Env -> Env
update (x,newval) [] = [(x,newval)]
update (x,newval) ((y,v) : e) | x == y = (x,newval) : e
                              | otherwise = (y,v) : update (x,newval) e

lookUp :: Vars -> Env -> Integer
lookUp x e = case (lookup x e) of
  Just v  -> v
  Nothing -> error ("Variable " ++ x ++ " not found in environment: " ++ show e)

evala :: Env -> AExpr -> Integer
evala env (Var x) = lookUp x env
evala env (Const v) = v
evala env (Add p1 p2) = evala env p1 + evala env p2
evala env (Sub p1 p2) = evala env p1 - evala env p2
evala env (Mul p1 p2) = evala env p1 * evala env p2
evala env (Div p1 p2) = evala env p1 `div` evala env p2
evala env (Exp p1 p2) = evala env p1 ^ evala env p2
evala env (Mod p1 p2) = evala env p1 `mod` evala env p2
-- added
evala env (LShift p1 p2) = evala env p1 `shiftL` fromIntegral (evala env p2)
evala env (RShift p1 p2) = evala env p1 `shiftR` fromIntegral (evala env p2)
evala env (Bin b) = b
evala env (Hex h) = h
evala env (FCall fn args) = evalaFuncCall env fn args
evala env (Sum es) = sum (map (evala env) es)

evalaFuncCall :: Env -> FName -> [AExpr] -> Integer
evalaFuncCall env fn args = case lookup fn fEnv of
  Nothing -> error $ "Function not found: " ++ fn
  Just (vars,body) ->
    let evaledArgs = map (evala env) args
        newEnv = zip vars evaledArgs
     in evala newEnv body


-- Fro
  -- F x y = x*x + y
  -- G z = F[2*z,z+1]
  -- G[5]
fEnv :: FEnv -- this might be the issue in hindsight
fEnv = [ ("F", (["x","y"],(Add (Mul (Var "x") (Var "x")) (Var "y"))))
       , ("G", (["z"],    (FCall "F" [Mul (Const 2) (Var "z"),Add (Var "z") (Const 1)])))]

evalb :: Env -> BExpr -> Bool
evalb _ TT = True
evalb _ FF = False
evalb e (And b1 b2) = evalb e b1 && evalb e b2
evalb e (Or  b1 b2) = evalb e b1 || evalb e b2
evalb e (Not b) = not $ evalb e b
evalb e (Eq  e1 e2) = evala e e1 == evala e e2
evalb e (Lt e1 e2)  = evala e e1 <  evala e e2
evalb e (Lte e1 e2) = evala e e1 <= evala e e2

exec :: Instr -> Env -> Env
exec (Assign x v) e = update (x, evala e v) e
exec (IfThenElse c i1 i2) e = if evalb e c then exec i1 e else exec i2 e
exec (While c i) e = if evalb e c then exec (While c i) (exec i e) else e
exec (Do []) e = e
exec (Do (i:is)) e = exec (Do is) (exec i e)
exec Nop e = e
exec (Return a) e = update ("",evala e a) e

execList :: [Instr] -> Env -> Env
execList [] = id
execList (i:is) = execList is . exec i

parseAExpr :: [Token] -> AExpr
parseAExpr ts = case sr [] ts of
  [PA e] -> e
  s      -> error ("Parse error:" ++ show s)

parseBExpr :: [Token] -> BExpr
parseBExpr ts = case sr [] ts of
  [PB e] -> e
  s      -> error ("Parse error:" ++ show s)

parseInstr :: [Token] -> Instr
parseInstr ts = case sr [] ts of
  [PI i] -> i
  s      -> error ("Parse error:" ++ show s) 

run :: [Instr] -> Integer
run p = case lookup "" (execList p []) of -- below Changed
   Just x  -> x
   Nothing -> error "No value returned."
 
sr :: [Token] -> [Token] -> [Token]
-- variables and constants
sr (VSym x : ts)     q = sr (PA (Var  x) : ts) q
sr (CSym x : ts)     q = sr (PA (Const x) : ts) q
sr (BSym True : ts)  q = sr (PB TT : ts) q
sr (BSym False : ts) q = sr (PB FF : ts) q
-- arithmetic operations
sr (PA p2 : BOp AddOp : PA p1 : ts) q = sr (PA (Add p1 p2) : ts) q
sr (PA p2 : BOp SubOp : PA p1 : ts) q = sr (PA (Sub p1 p2) : ts) q
sr (PA p2 : BOp MulOp : PA p1 : ts) q = sr (PA (Mul p1 p2) : ts) q
sr (PA p2 : BOp DivOp : PA p1 : ts) q = sr (PA (Div p1 p2) : ts) q
sr (PA p2 : BOp ModOp : PA p1 : ts) q = sr (PA (Mod p1 p2) : ts) q
sr (PA p2 : BOp ExpOp : PA p1 : ts) q = sr (PA (Exp p1 p2) : ts) q
-- added
sr (PA p2 : BOp LShiftOp : PA p1 : ts) q = sr (PA (LShift p1 p2) : ts) q
sr (PA p2 : BOp RShiftOp : PA p1 : ts) q = sr (PA (RShift p1 p2) : ts) q
sr (BinSym b : ts) q = sr (PA (Bin b) : ts) q
sr (HexSym h : ts) q = sr (PA (Hex h) : ts) q
sr (Comma : PA e : PList es : s)   q = sr (PList (e:es) : s) q
-- boolean operations
sr (PB p : UOp NotOp : ts)          q = sr (PB (Not p) : ts) q
sr (PB p2 : BOp AndOp : PB p1 : ts) q = sr (PB (And p1 p2) : ts) q
sr (PB p2 : BOp OrOp  : PB p1 : ts) q = sr (PB (Or  p1 p2) : ts) q
-- comparisons and assignment operation
sr (PA p2 : BOp EqOp : PA p1 : ts) q = sr (PB (Eq p1 p2) : ts) q
sr (PA p2 : BOp LtOp  : PA p1 : ts) q = sr (PB (Lt p1 p2) : ts)  q
sr (PA p2 : BOp LteOp : PA p1 : ts) q = sr (PB (Lte p1 p2) : ts) q
sr (PA p : AssignOp : PA (Var x) : ts) q = sr (PI (Assign x p) : ts) q
-- keywords
sr (PI i0 : PB b : Keyword WhileK : ts) q = sr (PI (While b i0) : ts) q
sr (Keyword NopK : ts)                  q = sr (PI Nop : ts) q
sr (PI i2 : Keyword ElseK : PI i1 : Keyword ThenK : PB b : Keyword IfK :ts) q
                                           = sr (PI (IfThenElse b i1 i2) : ts) q
-- sr (PI i2 : Keyword ElseK : Semi : PI i1 : Keyword ThenK : PB b : Keyword IfK :ts) q
--                                            = sr (PI (IfThenElse b i1 i2) : ts) q
-- sr (Semi : PI i2 : Keyword ElseK : Semi : PI i1 : Keyword ThenK : PB b : Keyword IfK :ts) q
--                                            = sr (PI (IfThenElse b i1 i2) : ts) q
sr (RBra : PList es : NSym fn : s)         q = sr (PA (FCall fn (reverse es)) : s)  q
sr (PA e : Keyword ReturnK : ts) q = sr (PI (Return e) : ts) q
-- parsing do blocks
sr (LBra : ts)                   q = sr (Block [] : LBra : ts) q
sr (LBrack : s)        q = sr (PList [] : s)  q
sr (Semi : PI i : Block is : ts) q = sr (Block (i:is) : ts)    q
sr (RBra : Block is : LBra : ts)        q = sr (PI (Do (reverse is)) : ts) q
sr (RBra : PI i : Block is : LBra : ts) q = sr (PI (Do (reverse (i:is))) : ts) q
-- parentheses
sr (RPar : PA p : LPar : ts)        q = sr (PA p : ts) q
sr (RPar : PB p : LPar : ts)        q = sr (PB p : ts) q
sr (RBra : PI p : LBra : ts)        q = sr (PI p : ts) q
sr (RBrack : PA e : PList es : s)  q = sr (RBrack : PList (e:es) : s) q
sr (RBrack : PList es : BOp SumOp : s)         q = sr (PA (Sum (reverse es)) : s)  q
sr (RBrack : PList es : NSym fn : s)         q = sr (PA (FCall fn (reverse es)) : s)  q

-- sr (RBra : PI i : ts)            q = sr (Block [i] : ts) q
-- sr (RBra : ts)                   q = sr (Block []  : ts) q
-- -- sr (RBra : Semi : PI i : ts) q = sr (Block [i] : ts) q
-- sr (Block is : Semi : PI i : ts) q = sr (Block (i:is) : ts) q
-- sr (Block is : LBra : ts)        q = sr (PI (Do is) : ts)   q
-- shift and return
sr s (i:q) = sr (i:s) q
sr s []    = s