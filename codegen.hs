module CodeGen where
    
import Parser
import Control.Monad
import Control.Monad.State
import Control.Monad.ST
import Data.List
import IO

import Lex -- убрать

genLabel :: State Int String
genLabel = do n <- get
              put (n+1)
              return ("label"++(show n))
              
data GenState = GS {
                    lNumber :: Int,
                    --locals :: [String],
                    owners :: [String],
                    regNumber :: Int,
                    subNumber :: Int,
                    inCycle :: Bool,
                    resReg :: Int,
                    writeAfter :: [String]
                    }
                
              

codeGen :: Program -> [String]
codeGen p = evalState (generate p) (GS 0 [] 1 1 False 0 [])

class AST a where
    generate :: a -> State GenState [String]
    
instance AST Program where
    generate (Error strs) = return strs
    generate (Ok dcls) = do begin <- return [".sub '__sub0' :main"]
                            dec <- liftM (concat) $ mapM generate $ fst l2
                            end <- return ["\t$I0 = 'main' ()", "\t.return ($IO)",".end"]
                            fn <- liftM concat $ mapM generate $ snd l2
                            return (begin ++ dec ++ end ++ fn)
        where l2 = partition (\d -> case d of
                                      FunDecl _ _ _ _ -> False
                                      _ -> True) dcls
    
instance AST Decl where
    generate (FunDecl t i plist block) = do    let name = fromId i
                                               s <- get
                                               put s{owners = name : (owners s)}
                                               let sign = ".sub '" ++ name ++ "' :outer('__sub0')"
                                               let techVars = ["\t.local int __cend","\t__cend = 0","\t.lex '__cend', __cend",
                                                               "\t.local int __res","\t__res = 0","\t.lex '__res', __res",
                                                               "\t.local int __fend","\t__fend = 0","\t.lex '__fend', fend"]
                                               params <- liftM concat $ mapM generate plist
                                               body <- generate block
                                               s1 <- get
                                               let ending = ".end" : (writeAfter s1)
                                               put s1{owners = tail $ owners s1, writeAfter = []}
                                               return (["",sign] ++ params ++ body ++ ending)
    generate (VarDecl _ id) = return ["\t.local int " ++ name, "\t.lex '" ++ name ++"', " ++ name]
        where name = fromId id
    generate (MDecl _ id mnum) = return ["\t.local pmc " ++ name,
                                         "\t " ++ name ++ " = new \"FixedIntegerArray\"",
                                         "\t " ++ name ++ " = " ++ size,
                                         "\t.lex '" ++ name ++ "', " ++ name
                                         ]
        where name = fromId id
              size = show $ fromMNum mnum
              
instance AST ParamDecl where
    generate (ParamVarDecl t i) = return ["\t.param int " ++ name,"\t.lex '" ++ name ++ "', " ++ name]
        where name = (fromId i)
    generate (ParamMDecl t i) = return ["\t.param pmc " ++ name,"\t.lex '" ++ name ++ "', " ++ name]
        where name = (fromId i)
    
instance AST Stmt where
    generate (Block decls stmts) = do s <- get
                                      let name = "__sub" ++ (show $ subNumber s)
                                      let sign = ".sub '" ++ name ++ "':outer('" ++ (head $ owners s) ++ "')" 
                                      put s{owners = name : (owners s), subNumber = 1 + subNumber s }
                                      loc <- liftM concat $ mapM generate decls
                                      body <- liftM concat $ mapM generate stmts
                                      s1 <- get
                                      
                                      if inCycle s1
                                      then do put s1{owners = tail $ owners s1, writeAfter = writeAfter s1 ++ ["",sign] ++ loc ++ body ++[".end"]}
                                              return [""] 
                                      else do let label = "label" ++ (show $ lNumber s1)
                                              put s1{owners = tail $ owners s1,lNumber = 1 + lNumber s1, writeAfter = writeAfter s1 ++ ["",sign] ++ loc ++ body ++[".end"]}
                                              return ["\t" ++ name ++ "()", "\t unless __fend goto " ++ label, "\t return (__res)", label ++ ":"]
                                   
    generate WriteLn = return ["\tsay \"\""]
    generate (Write exp) = do count <- generate exp
                              s <- get
                              if exprType exp == TypeInt
                              then return (count ++ ["\tsay $I" ++ (show $ resReg s)])
                              else do put s{regNumber = 1 + regNumber s}
                                      return (count ++["\t$S" ++ (show $ regNumber s) ++ " = chr $I" ++ (show $ resReg s),
                                                       "\tsay $S" ++ (show $ regNumber s)])
    generate _ = return ["\tStmt"]
    
    
instance AST Expr where
        generate (NExpr val t) = do s <- get
                                    put s{resReg = regNumber s, regNumber = 1 + regNumber s}
                                    return ["\t$I" ++ (show $ regNumber s) ++ " = " ++ (show val)] 
        generate _ = do s <- get
                        put s{resReg = regNumber s, regNumber = 1 +regNumber s}
                        return ["\t$I" ++ (show $ regNumber s) ++ " = 0"]
               
