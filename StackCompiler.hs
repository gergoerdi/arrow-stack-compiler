{-# LANGUAGE Arrows, EmptyDataDecls #-}
module StackCompiler where

import Prelude hiding (print)    
    
import Control.Arrow
import Control.Arrow.Transformer.Writer
import Control.Arrow.Operations
import Control.Monad.Identity

--- Type-level naturals    
data Z
data S n

--- Opcodes of the target machine    
data Op = Push Register
        | Pop Register
        | Nop
        | Add Register Register
        | Mul Register Register
        | Set Register Int
        | Print Register
        deriving Show

data Register = EAX | EBX deriving (Show, Eq)
    
--- Input language
data Expr = IntLit Int
          | Expr :+: Expr
          | Expr :*: Expr
          deriving Show


--- The assembler arrow
type Machine b c = WriterArrow [Op] (Kleisli Identity) b c

output :: Op -> Machine n m
output op = proc _ -> do
              write -< [op]
              returnA -< undefined


--- CPU opcodes with stack specification
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
add target param = output $ Add target param

mul :: Register -> Register -> Machine n n
mul target param = output $ Mul target param

           
--- Derived operations
binOp :: (Register -> Register -> Machine n n) -> Machine (S (S n)) (S n)
binOp op = pop EAX >>>
           pop EBX >>>
           op EAX EBX >>>
           push EAX


--- Compiler                     
compileExpr :: Expr -> Machine n (S n)
compileExpr (IntLit n) = set EAX n >>>
                         push EAX
                              
compileExpr (e :+: e') = compileExpr e >>>
                         compileExpr e' >>>
                         binOp add
                              
compileExpr (e :*: e') = compileExpr e >>>
                         compileExpr e' >>>
                         binOp mul

--- Runner for compiler
assemble :: Machine n n -> [Op]
assemble prog = snd $ runIdentity (runKleisli (elimWriter prog) initial)
    where initial = undefined :: n
       
--- Optimizer : Eliminates [push r, pop r] and [pop r, push r]
optimize :: [Op] -> [Op]
optimize = fst . optimize'
    where optimize' :: [Op] -> ([Op], Bool)
          optimize' ((Push r):(Pop r'):ops) | r == r' = (fst (optimize' ops), True)
          optimize' ((Pop r):(Push r'):ops) | r == r' = (fst (optimize' ops), True)
          optimize' (op:ops) = let (ops', optimized) = optimize' ops
                               in if optimized then optimize' (op:ops')
                                  else (op:ops', False)
          optimize' [] = ([], False)

--- Demo                         
expr = ((IntLit 10) :+: (IntLit 5)) :*: (IntLit 3)                   
test = assemble (compileExpr expr >>> print)
