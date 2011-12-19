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
                            end <- return ["\t$I0 = 'main' ()", "\t.return ($I0)",".end"]
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
                                                               "\t.local int __fend","\t__fend = 0","\t.lex '__fend', __fend"]
                                               params <- liftM concat $ mapM generate plist
                                               body <- generate block
                                               s1 <- get
                                               let ending = ".end" : (writeAfter s1)
                                               put s1{owners = tail $ owners s1, writeAfter = []}
                                               return (["",sign] ++ techVars ++ params ++ body ++ ending)
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
                                              return ["\t" ++ name ++ "()", "\tunless __fend goto " ++ label, "\treturn (__res)", label ++ ":"]
                                   
    generate WriteLn = return ["\tsay \"\""]
    generate (Write exp) = do count <- generate exp
                              s <- get
                              if exprType exp == TypeInt
                              then return (count ++ ["\tsay $I" ++ (show $ resReg s)])
                              else do put s{regNumber = 1 + regNumber s}
                                      return (count ++["\t$S" ++ (show $ regNumber s) ++ " = chr $I" ++ (show $ resReg s),
                                                       "\tsay $S" ++ (show $ regNumber s)])
    generate (Stmt expr) = generate expr                                                        
    generate _ = return ["\tStmt"]
    
    
instance AST Expr where
        generate (NExpr val t) = do s <- get
                                    put s{resReg = regNumber s, regNumber = 1 + regNumber s}
                                    return ["\t$I" ++ (show $ regNumber s) ++ " = " ++ (show val)] 

        -- Если вызвана, то name гарантированно не массив и не функция
        generate (IExpr name t ) = do s <- get
                                      put s{resReg = regNumber s, regNumber = 1 + regNumber s}
                                      return ["\t" ++ name ++ " = find_lex '" ++ name ++ "'",
                                              "\t$I" ++ (show $ regNumber s) ++ " = " ++ name]
                                    
        
        generate (Assign id expr t) = do e <- generate expr
                                         s <- get
                                         let assign = (e ++ ["\t" ++ name ++ " = $I" ++ (show $ resReg s)])      
                                         put s{resReg = regNumber s, regNumber = 1 + regNumber s}
                                         let m = if t == TypeInt
                                                 then []
                                                 else ["\t" ++ name ++ " = " ++ name ++ " % 256"]
                                         return (assign ++ m ++
                                                 [ "\tstore_lex '" ++ name ++ "', " ++ name,
                                                   "\t$I" ++ (show $ regNumber s) ++ " = " ++ name])
                     where name = fromId id
        generate (Plus expr1 expr2 t) = do e1 <- generate expr1
                                           s1 <- get
                                           e2 <- generate expr2
                                           s2 <- get
                                           put s2{resReg = regNumber s2, regNumber = 1 + regNumber s2}
                                           if t == TypeInt
                                           then return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " + $I" ++ (show $ resReg s2)])
                                           else return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " + $I" ++ (show $ resReg s2),
                                                                     "\t$I"  ++ (show $ regNumber s2) ++ " = $I" ++ (show $ regNumber s2) ++ " % 256"])
        generate (Mult expr1 expr2 t) = do e1 <- generate expr1
                                           s1 <- get
                                           e2 <- generate expr2
                                           s2 <- get
                                           put s2{resReg = regNumber s2, regNumber = 1 + regNumber s2}
                                           if t == TypeInt
                                           then return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " * $I" ++ (show $ resReg s2)])
                                           else return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " * $I" ++ (show $ resReg s2),
                                                                     "\t$I"  ++ (show $ regNumber s2) ++ " = $I" ++ (show $ regNumber s2) ++ " % 256"])
       
        generate (Div  expr1 expr2 t) = do e1 <- generate expr1
                                           s1 <- get
                                           e2 <- generate expr2
                                           s2 <- get
                                           put s2{resReg = regNumber s2, regNumber = 1 + regNumber s2}
                                           if t == TypeInt
                                           then return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " / $I" ++ (show $ resReg s2)])
                                           else return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " / $I" ++ (show $ resReg s2),
                                                                     "\t$I"  ++ (show $ regNumber s2) ++ " = $I" ++ (show $ regNumber s2) ++ " % 256"])
        generate (Minus  expr1 expr2 t) = do e1 <- generate expr1
                                             s1 <- get
                                             e2 <- generate expr2
                                             s2 <- get
                                             put s2{resReg = regNumber s2, regNumber = 1 + regNumber s2}
                                             if t == TypeInt
                                             then return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " - $I" ++ (show $ resReg s2)])
                                             else return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " - $I" ++ (show $ resReg s2),
                                                                       "\t$I"  ++ (show $ regNumber s2) ++ " = $I" ++ (show $ regNumber s2) ++ " % 256"])
        
        generate (Greater  expr1 expr2 t) = do e1 <- generate expr1
                                               s1 <- get
                                               e2 <- generate expr2
                                               s2 <- get
                                               put s2{resReg = regNumber s2, regNumber = 1 + regNumber s2}                                              
                                               return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " > $I" ++ (show $ resReg s2)])                                               
        
        generate (Less  expr1 expr2 t) = do e1 <- generate expr1
                                            s1 <- get
                                            e2 <- generate expr2
                                            s2 <- get
                                            put s2{resReg = regNumber s2, regNumber = 1 + regNumber s2}                                              
                                            return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " < $I" ++ (show $ resReg s2)])   

        generate (GEq  expr1 expr2 t) = do e1 <- generate expr1
                                           s1 <- get
                                           e2 <- generate expr2
                                           s2 <- get
                                           put s2{resReg = regNumber s2, regNumber = 1 + regNumber s2}                                              
                                           return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " >= $I" ++ (show $ resReg s2)])                                               
        
        generate (LEq  expr1 expr2 t) = do e1 <- generate expr1
                                           s1 <- get
                                           e2 <- generate expr2
                                           s2 <- get
                                           put s2{resReg = regNumber s2, regNumber = 1 + regNumber s2}                                              
                                           return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " <= $I" ++ (show $ resReg s2)])                                               
        
        generate (Eq  expr1 expr2 t) = do e1 <- generate expr1
                                          s1 <- get
                                          e2 <- generate expr2
                                          s2 <- get
                                          put s2{resReg = regNumber s2, regNumber = 1 + regNumber s2}                                              
                                          return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " == $I" ++ (show $ resReg s2)])                                               
        
        generate (NEq  expr1 expr2 t) = do e1 <- generate expr1
                                           s1 <- get
                                           e2 <- generate expr2
                                           s2 <- get
                                           put s2{resReg = regNumber s2, regNumber = 1 + regNumber s2}                                              
                                           return (e1 ++ e2 ++ ["\t$I" ++ (show $ regNumber s2) ++ " = $I" ++ (show $ resReg s1) ++ " != $I" ++ (show $ resReg s2)])                                               
        
        generate _ = do s <- get
                        put s{resReg = regNumber s, regNumber = 1 +regNumber s}
                        return ["\t$I" ++ (show $ regNumber s) ++ " = 0"]
               
