{-# LANGUAGE Arrows, EmptyDataDecls #-}
module Compiler where

import Prelude hiding (print)    
    
import Control.Arrow
import Control.Arrow.Transformer.Writer
import Control.Arrow.Operations
import Control.Monad
import Control.Monad.Identity

data Z
data S n

data Register = EAX | EBX deriving (Show, Eq)
    
data Op = Push Register
        | Pop Register
        | Nop
        | Add Register Register
        | Mul Register Register
        | Set Register Int
        | Print Register
        deriving Show

data Expr = IntLit Int
          | Expr :+: Expr
          | Expr :*: Expr
          deriving Show

type Machine b c = WriterArrow [Op] (Kleisli Identity) b c

output :: Op -> Machine n m
output op = proc _ -> do
              write -< [op]
              returnA -< (undefined :: m)


--- CPU opcodes with stack manipulation information              
push :: Register -> Machine n (S n)
push reg = output $ Push reg

pop :: Register -> Machine (S n) n
pop reg = output $ Pop reg

nop :: Machine n n
nop = output $ Nop

set :: Register -> Int -> Machine n n
set r n = output $ Set r n

print = pop EAX >>>
        output (Print EAX)

add :: Register -> Register -> Machine n n
add r r' = output $ Add r r'

mul :: Register -> Register -> Machine n n
mul r r' = output $ Mul r r'

           
--- Derived operations
stackBinOp :: (Register -> Register -> Machine n n) -> Machine (S (S n)) (S n)
stackBinOp op = pop EAX >>>
                pop EBX >>>
                op EAX EBX >>>
                push EAX


--- Compiler                     
compileExpr :: Expr -> Machine n (S n)
compileExpr (IntLit n) = set EAX n >>>
                         push EAX
                              
compileExpr (e :+: e') = compileExpr e >>>
                         compileExpr e' >>>
                         stackBinOp add
                              
compileExpr (e :*: e') = compileExpr e >>>
                         compileExpr e' >>>
                         stackBinOp mul

--- Runner for compiler                                    
collect :: Machine n n -> [Op]
collect prog = snd $ runIdentity (runKleisli (elimWriter prog) initial)
    where initial = undefined :: n
       
expr = ((IntLit 10) :+: (IntLit 5)) :*: (IntLit 3)                   
test = collect (compileExpr expr >>> print)

optimize :: [Op] -> [Op]
optimize = fst . optimize'
    where optimize' :: [Op] -> ([Op], Bool)
          optimize' ((Push r):(Pop r'):ops) | r == r' = (fst (optimize' ops), True)
          optimize' ((Pop r):(Push r'):ops) | r == r' = (fst (optimize' ops), True)
          optimize' (op:ops) = let (ops', optimized) = optimize' ops
                               in if optimized then optimize' (op:ops')
                                  else (op:ops', False)
          optimize' [] = ([], False)
                                              
