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
                    deepInCycle :: Bool,
                    resReg :: Int,
                    writeAfter :: [String]
                    }
                
              

codeGen :: Program -> [String]
codeGen p = evalState (generate p) (GS 0 [] 1 1 False False 0 [])

class AST a where
    generate :: a -> State GenState [String]
    
instance AST Program where
    generate (Error strs) = return strs
    generate (Ok dcls) = do begin <- return [".loadlib 'io_ops'",
                                             "",
                                             ".sub 'sub0' :main"]
                            dec <- liftM (concat) $ mapM generate $ fst l2
                            end <- return ["\t$I0 = '__main' ()", "\t.return ($I0)",".end"]
                            fn <- liftM concat $ mapM generate $ snd l2
                            return (begin ++ dec ++ end ++ fn)
        where l2 = partition (\d -> case d of
                                      FunDecl _ _ _ _ -> False
                                      _ -> True) dcls
    
instance AST Decl where
    generate (FunDecl t i plist block) = do    let name = "__" ++ ( fromId i )
                                               s <- get
                                               put s{owners = name : (owners s)}
                                               let sign = ".sub '" ++ name ++ "' :outer('sub0')"
                                               let techVars = ["\t.local int cend","\tcend = 0","\t.lex 'cend', cend",
                                                               "\t.local int res","\tres = 0","\t.lex 'res', res",
                                                               "\t.local int fend","\tfend = 0","\t.lex 'fend', fend"]
                                               params <- liftM concat $ mapM generate plist
                                               body <- generate block
                                               s1 <- get
                                               let ending = ".end" : (writeAfter s1)
                                               put s1{owners = tail $ owners s1, writeAfter = []}
                                               return (["",sign] ++ techVars ++ params ++ body ++ ending)
    generate (VarDecl _ id) = return ["\t.local int " ++ name, "\t.lex '" ++ name ++"', " ++ name]
        where name = "__" ++ ( fromId id )
    generate (MDecl _ id mnum) = return ["\t.local pmc " ++ name,
                                         "\t " ++ name ++ " = new \"FixedIntegerArray\"",
                                         "\t " ++ name ++ " = " ++ size,
                                         "\t.lex '" ++ name ++ "', " ++ name
                                         ]
        where name = "__" ++ ( fromId id )
              size = show $ fromMNum mnum
              
instance AST ParamDecl where
    generate (ParamVarDecl t i) = return ["\t.param int " ++ name,"\t.lex '" ++ name ++ "', " ++ name]
        where name = "__" ++ ( fromId i )
    generate (ParamMDecl t i) = return ["\t.param pmc " ++ name,"\t.lex '" ++ name ++ "', " ++ name]
        where name = "__" ++ ( fromId i )
    
instance AST Stmt where
    generate (While expr stmt) = do ss <- get
                                    put ss{inCycle = True}
                                    s1 <- generate stmt
                                    e <- generate expr
                                    s <- get 
                                    put s{lNumber = 2 + lNumber s,inCycle = False}
                                    return ( ["label" ++ (show $ lNumber s) ++ ":",
                                              "\t.local int cend",
                                              "\tcend = 0",
                                              "\t.lex 'cend', cend"] ++ 
                                             e ++ 
                                             ["\t$I" ++ (show $ resReg s) ++ " = not $I" ++ (show $ resReg s),
                                              "\t$I" ++ (show $ resReg s) ++ " = or cend, $I" ++ (show $ resReg s),
                                              "\tif $I" ++ (show $ resReg s) ++ " goto label" ++ (show $ lNumber s + 1)] ++
                                             s1 ++
                                             ["\tgoto label" ++ (show $ lNumber s),
                                              "label" ++ (show $ lNumber s + 1) ++ ":"])

    generate (IfElse expr stmt1 stmt2) = do s1 <- generate stmt1
                                            s2 <- generate stmt2
                                            e <- generate expr
                                            s <- get
                                            put s{lNumber = 2 + lNumber s}
                                            return (e ++
                                                    ["\tif $I" ++ (show $ resReg s) ++ " goto label" ++ (show $ lNumber s)] ++
                                                    s2 ++
                                                    ["\tgoto label" ++ (show $ lNumber s + 1),
                                                     "label" ++ (show $ lNumber s) ++ ":"] ++
                                                    s1 ++
                                                    ["label" ++ (show $ lNumber s + 1) ++ ":"])
                                            
    generate (Break) = do s <- get
                          if deepInCycle s
                          then return ["\t.local int cend",
                                       "\tcend = 1",
                                       "\tstore_lex 'cend', cend",
                                       "\t.return()"]
                          else return ["\t.local int cend",
                                       "\tcend = 1",
                                       "\tstore_lex 'cend', cend"]
    
    generate (Block decls stmts) = do s <- get
                                      let name = "sub" ++ (show $ subNumber s)
                                      let sign = ".sub '" ++ name ++ "':outer('" ++ (head $ owners s) ++ "')" 
                                      if inCycle s
                                      then put s{owners = name : (owners s), subNumber = 1 + subNumber s }
                                      else put s{owners = name : (owners s), subNumber = 1 + subNumber s, deepInCycle = True }
                                      loc <- liftM concat $ mapM generate decls
                                      body <- liftM concat $ mapM generate stmts
                                      s1 <- get
                                      if deepInCycle s1
                                      then do let label = "label" ++ (show $ lNumber s1)
                                              put s1{owners = tail $ owners s1,regNumber = 1 + regNumber s1, lNumber = 1 + lNumber s1, writeAfter = ["",sign] ++ loc ++ body ++[".end"] ++ writeAfter s1}
                                              return ["\t" ++ name ++ "()",
                                                      "\t.local int cend",
                                                      "\tcend = find_lex 'cend'",
                                                      "\t.local int fend",
                                                      "\tfend = find_lex 'fend'",
                                                      "\t.local int res",
                                                      "\tres = find_lex 'res'",
                                                      "\t$I" ++ (show $ regNumber s1) ++ " = or cend, fend",
                                                      "\tunless $I" ++ (show $ regNumber s1) ++ " goto " ++ label,
                                                      "\t.return(res)",
                                                      label ++ ":"] 
                                      else do let label = "label" ++ (show $ lNumber s1)
                                              put s1{owners = tail $ owners s1,lNumber = 1 + lNumber s1, writeAfter =  ["",sign] ++ loc ++ body ++[".end"] ++ writeAfter s1, deepInCycle = False}
                                              return ["\t" ++ name ++ "()",
                                                      "\t.local int fend",
                                                      "\tcend = find_lex 'fend'",                                              
                                                      "\tunless fend goto " ++ label, 
                                                      "\t.return ()", 
                                                      label ++ ":"]
                                   
    generate (Ret expr) = do e <- generate expr
                             s <- get
                             return (e ++ ["\t.local int res",
                                           "\tres = $I" ++ (show $ resReg s),
                                           "\tstore_lex 'res', res",
                                           "\t.local int fend",
                                           "\tfend = 1",
                                           "\tstore_lex 'fend', fend",
                                           "\t.return()"])
    
    generate (Read id t) = do s <- get
                              put s{regNumber = 2 + regNumber s}
                              let part1 = ["\t$P" ++ (show $ regNumber s) ++ " = getstdin",
                                           "\t$S" ++ (show $ regNumber s + 1) ++ " = readline $P" ++ (show $ regNumber s),
                                           "\t.local int " ++ name]
                              if t == TypeInt
                              then return (part1 ++ ["\t" ++ name ++ " = $S" ++ (show $ regNumber s + 1),
                                                     "\tstore_lex '" ++ name ++ "', " ++ name])
                              else return (part1 ++ ["\t" ++ name ++ " = ord $S" ++ (show $ regNumber s + 1),
                                                     "\tstore_lex '" ++ name ++ "', " ++ name])
                   where name = "__" ++ ( fromId id )

    generate WriteLn = return ["\tsay \"\""]
    generate (Write exp) = do count <- generate exp
                              s <- get
                              if exprType exp == TypeInt
                              then return (count ++ ["\tprint $I" ++ (show $ resReg s)])
                              else do put s{regNumber = 1 + regNumber s}
                                      return (count ++["\t$S" ++ (show $ regNumber s) ++ " = chr $I" ++ (show $ resReg s),
                                                       "\tprint $S" ++ (show $ regNumber s)])
    generate (Stmt expr) = generate expr                  
    generate Nop = return []    
    generate _ = return ["\tStmt"]
    
    
instance AST Expr where
        generate (NExpr val t) = do s <- get
                                    put s{resReg = regNumber s, regNumber = 1 + regNumber s}
                                    return ["\t$I" ++ (show $ regNumber s) ++ " = " ++ (show val)] 

        generate (IExpr name t ) = do s <- get
                                      put s{resReg = regNumber s, regNumber = 1 + regNumber s}
                                      if t /= TypeInt && t/=TypeChar
                                      then return ["\t.local pmc " ++ "__" ++ name,
                                                   "\t" ++ "__" ++ name ++ " = find_lex '" ++ "__" ++ name ++ "'",
                                                   "\t$P" ++ (show $ regNumber s) ++ " = " ++ "__" ++ name]
                                      else return ["\t.local int " ++ "__" ++ name,
                                                   "\t" ++ "__" ++ name ++ " = find_lex '" ++ "__" ++ name ++ "'",
                                                   "\t$I" ++ (show $ regNumber s) ++ " = " ++ "__" ++ name]
                                    
        generate (Func id params t) = do s <- get
                                         let (ecount, ress) = unzip $ snd $ foldl (\(r,rs) p -> let (v,s) = runState (generate p) r in 
                                                                                               (s,rs ++ [(v,s)])) (s,[])  params
                                         let actpars = zipWith (\p r -> if exprType p == TypeInt || exprType p == TypeChar
                                                                        then "$I" ++ (show $ resReg r)
                                                                        else "$P" ++ (show $ resReg r)) params ress
                                         let sign = concat $ intersperse ", " actpars
                                         let s1 = last ress
                                         put s1{resReg = regNumber s1, regNumber = 1 + regNumber s1}
                                         if t == TypeInt
                                         then return ((concat ecount) ++ ["\t$I" ++ (show $ regNumber s1) ++ " = " ++ name ++ "(" ++ sign ++ ")"])
                                         else return ((concat ecount) ++ ["\t$I" ++ (show $ regNumber s1) ++ " = " ++ name ++ "(" ++ sign ++ ")",
                                                                         "\t$I" ++ (show $ regNumber s1) ++ " = " ++ "$I" ++ (show $ regNumber s1) ++ " % 256"])
        
                    where name = "__" ++ ( fromId id )
        
        generate (M id expr t) = do e <- generate expr
                                    s <- get
                                    put s{resReg = regNumber s, regNumber = 1 + regNumber s}
                                    return (e ++ ["\t.local pmc " ++ name,
                                                 "\t" ++ name ++ " = find_lex '" ++ name ++ "'",
                                                 "\t$I" ++ (show $ regNumber s) ++ " = " ++ name ++ "[$I" ++ (show $ resReg s) ++ "]"])
                     where name = "__" ++ ( fromId id )
        
        generate (Assign id expr t) = do e <- generate expr
                                         s <- get
                                         let assign = (e ++ ["\t.local int " ++ name,
                                                             "\t" ++ name ++ " = $I" ++ (show $ resReg s)])      
                                         put s{resReg = regNumber s, regNumber = 1 + regNumber s}
                                         let m = if t == TypeInt
                                                 then []
                                                 else ["\t" ++ name ++ " = " ++ name ++ " % 256"]
                                         return (assign ++ m ++
                                                 [ "\tstore_lex '" ++ name ++ "', " ++ name,
                                                   "\t$I" ++ (show $ regNumber s) ++ " = " ++ name])
                     where name = "__" ++ ( fromId id )
        
        generate (MAssign id expr1 expr2 t) = do e1 <- generate expr1
                                                 s1 <- get
                                                 e2 <- generate expr2
                                                 s2 <- get
                                                 let assign = (e1 ++ e2 ++ ["\t.local pmc " ++ name,
                                                                            "\t" ++ name ++ " = find_lex '" ++ name ++"'", 
                                                                            "\t" ++ name ++ "[$I" ++ ( show $ resReg s1) ++ "] = $I" ++ (show $ resReg s2)])
                                                 put s2{resReg = regNumber s2, regNumber = 1 + regNumber s2}
                                                 let m = if t == TypeInt
                                                         then []
                                                         else ["\t" ++ name ++ "[$I" ++ (show $ resReg s1) ++ "] = " ++ name ++ "[$I" ++ (show $ resReg s1) ++ "] % 256" ]
                                                 return (assign ++ m ++
                                                         ["\tstore_lex '" ++ name ++ "'," ++ name,
                                                          "\t$I" ++ (show $ regNumber s2) ++ " = " ++ name ++ "[$I" ++ (show $ resReg s1) ++ "]"])        
                                    
                     where name = "__" ++ ( fromId id )
        
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
        
        generate (Negate  expr t) = do e <- generate expr
                                       s <- get
                                       put s{resReg = regNumber s, regNumber = 1 + regNumber s}                                              
                                       return (e ++ ["\t$I" ++ (show $ regNumber s) ++ " = - $I" ++ (show $ resReg s)])                                               
        
        generate (Not  expr t) = do e <- generate expr
                                    s <- get
                                    put s{resReg = regNumber s, regNumber = 1 + regNumber s}                                              
                                    return (e ++ ["\t$I" ++ (show $ regNumber s) ++ " = not $I" ++ (show $ resReg s)])         
        generate _ = do s <- get
                        put s{resReg = regNumber s, regNumber = 1 +regNumber s}
                        return ["\t$I" ++ (show $ regNumber s) ++ " = 0"]
               
