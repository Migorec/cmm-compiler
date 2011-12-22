{
module Parser where
import Prelude hiding (lookup)
import Lex

import Data.Map hiding(map,lookup)
import qualified Data.Map as Map   (lookup)  
}

%attributetype {MyAttr a}
%attribute value { a }
%attribute errors {[String]}
%attribute itable { [SymTable]} -- Наследуемая таблица символов
%attribute stable { [SymTable]} -- Синтезируемая таблица символов
%attribute atype {Type}
%attribute mtype {Maybe Type} -- промежуточный. чтобы два раза не искать тип по таблицам
%attribute position { AlexPosn }
%attribute incycle { Bool }

%name parse Program
%name parseExpr Expr
%tokentype { Token }
%error { parseError}

%token
        eof {TEOF}
        int {TInt $$}
        char {TChar $$}
        if {TIf $$}
        else {TElse $$}
        while {TWhile $$}
        write {TWrite $$}
        writeln {TWriteLn $$}
        read {TRead $$}
        return {TReturn $$}
        break {TBreak $$}
        numc {TNumConst a b }
        charc {TCharConst a b }
        id {TId a b}
        ',' {TComma $$}
--        '.' {TDot a}
        ';' {TSemicolon $$}
        '(' {TLPar $$ }
        ')' {TRPar $$ }
        '[' {TLBracket $$}
        ']' {TRBracket $$}
        '{' {TLCurly $$}
        '}' {TRCurly $$}
        '=' {TAssign $$}
        '!' {TNot $$}
        '-' {TMinus $$}
        '+' {TPlus $$}
        '*' {TMult $$}
        '/' {TDiv $$}
        '!=' {TNEq $$}
        '<' {TLess $$}
        '>' {TGreater $$}
        '<=' {TLEq $$}
        '>=' {TGEq $$}
        '&&' {TAnd $$}
        '||' {TOr $$}
        '==' {TEq $$}     

      
%right '='
%left '||'
%left '&&'
%left '==' '!='
%left '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%right NEG '!'

%%
Program             : DeclList eof                           {$$ = if $$.errors == [] then Ok $1 else Error $$.errors; 
                                                              $$.stable = $1.stable;
                                                              $1.itable = [empty];
                                                              $$.mtype = lookup "main" $$.stable;
                                                              $$.errors = case $$.mtype of
                                                                          {Nothing -> $1.errors ++ ["error: there should be function 'main()'"];
                                                                           Just (TypeFunction _ args) -> if length args == 0
                                                                                                         then $1.errors
                                                                                                         else $1.errors ++ ["error: function main should have no arguments"];
                                                                           _ -> $1.errors ++ ["error: 'main' should be declared as function"]}}
DeclList            : Decl                                  {$$ = [$1];
                                                             $$.stable = $1.stable;
                                                             $1.itable = $$.itable;
                                                             $$.errors = $1.errors}
                    | Decl DeclList                         {$$ = $1:$2;
                                                             $$.stable = $2.stable;
                                                             $1.itable = $$.itable;
                                                             $2.itable = $1.stable;
                                                             $$.errors = $1.errors ++ $2.errors}
Decl                : VarDecl                               {$$ = $1;
                                                             $$.stable = $1.stable;
                                                             $1.itable = $$.itable;
                                                             $$.errors = $1.errors}
                    | FunDecl                               {$$ = $1;
                                                             $$.stable = $1.stable;
                                                             $1.itable = $$.itable;
                                                             $$.errors = $1.errors}
FunDecl             : Type Id '(' ParamDeclList ')'Block    {$$ = FunDecl $1 $2 $4 $6;
                                                             $$.stable = if member (fromId $2) $ head $$.itable
                                                                         then $$.itable
                                                                         else (insert (fromId $2) (TypeFunction $1 (map pDecl2Type $4)) $ head $$.itable):(tail $$.itable);
                                                             $$.errors = let title = if $4.errors ++ $6.errors == []
                                                                                     then []
                                                                                     else ["In function '" ++ fromId $2 ++ "':"] in
                                                                         if member (fromId $2) $ head $$.itable
                                                                         then [(show$lineNumber $2.position)++":"++(show$colNumber $2.position)++": error: symbol '"++(fromId $2)++"' redeclared"] ++ title ++ $4.errors ++ $6.errors
                                                                         else title ++ $4.errors ++ $6.errors;
                                                             $4.itable = [empty];
                                                             $6.itable = $4.stable ++ $$.stable;
                                                             $6.incycle = False
                                                             }
ParamDeclList       : {-empty-}                             {$$ = [];
                                                             $$.stable = $$.itable;
                                                             $$.errors = []}
                    | ParamDeclListTail                     {$$ = $1;
                                                             $1.itable = $$.itable;
                                                             $$.stable = $1.stable;
                                                             $$.errors = $1.errors}
ParamDeclListTail   : ParamDecl                             {$$ = [$1];
                                                             $1.itable = $$.itable;
                                                             $$.stable = $1.stable;
                                                             $$.errors = $1.errors}
                    | ParamDecl ',' ParamDeclListTail       {$$ = $1:$3;
                                                             $1.itable = $$.itable;
                                                             $3.itable = $1.stable;
                                                             $$.stable = $3.stable;
                                                             $$.errors = $1.errors ++ $3.errors}
ParamDecl           : Type Id                               {$$ = ParamVarDecl $1 $2;
                                                             $$.stable = if member (fromId $2) $ head $$.itable
                                                                         then $$.itable
                                                                         else if $1 == TypeInt
                                                                              then (insert (fromId $2) TypeInt $ head $$.itable):(tail $$.itable)
                                                                              else (insert (fromId $2) TypeInt $ head $$.itable):(tail $$.itable);
                                                             $$.errors = if member (fromId $2) $ head $$.itable
                                                                         then [(show$lineNumber $2.position)++":"++(show$colNumber $2.position)++": error: redifinition of parametr '"++(fromId $2)++"'"]
                                                                         else []}
                    | Type Id '[' ']'                       {$$ = ParamMDecl $1 $2;
                                                             $$.stable = if member (fromId $2) $ head $$.itable
                                                                         then $$.itable
                                                                         else  if $1 == TypeInt
                                                                               then (insert (fromId $2) (TypeArray TypeInt) $ head $$.itable):(tail $$.itable)
                                                                               else (insert (fromId $2) (TypeArray TypeChar) $ head $$.itable):(tail $$.itable);
                                                             $$.errors = if member (fromId $2) $ head $$.itable
                                                                         then [(show$lineNumber  $2.position)++":"++(show$colNumber $2.position)++": error: redifinition of parametr '"++(fromId $2)++"'"]
                                                                         else []}
VarDeclList         : {-empty-}                             {$$ = [];
                                                             $$.stable = $$.itable;
                                                             $$.errors = []}
                    | VarDeclListTail                       {$$ = $1;
                                                             $$.stable = $1.stable;
                                                             $1.itable = $$.itable;
                                                             $$.errors = $1.errors}
VarDeclListTail     : VarDecl                               {$$ = [$1];
                                                             $$.stable = $1.stable;
                                                             $1.itable = $$.itable;
                                                             $$.errors = $1.errors
                                                             }
                    | VarDecl VarDeclListTail               {$$ = $1:$2;
                                                             $1.itable = $$.itable;
                                                             $2.itable = $1.stable;
                                                             $$.stable = $2.stable;
                                                             $$.errors = $1.errors ++ $2.errors}
Block               : '{' VarDeclList StmtList '}'          {$$ = Block $2 $3;
                                                             $2.itable = $$.itable;
                                                             $3.itable = $2.stable;
                                                             $$.stable = $2.stable;
                                                             $$.errors = $2.errors ++ $3.errors;
                                                             $3.incycle = $$.incycle}
VarDecl             : Type Id ';'                           {$$ = VarDecl $1 $2;
                                                             $$.position = $1.position;
                                                             $$.stable = if  member (fromId $2) $ head $$.itable
                                                                         then $$.itable
                                                                         else if $1 == TypeInt
                                                                              then (insert (fromId $2) TypeInt $ head $$.itable):(tail $$.itable)
                                                                              else (insert (fromId $2) TypeChar $ head $$.itable):(tail $$.itable);
                                                             $$.errors = if member (fromId $2) $ head $$.itable
                                                                         then [(show$lineNumber $2.position)++":"++(show$colNumber $2.position)++": error: symbol '"++(fromId $2)++"' redeclared"]
                                                                         else []}
                    | Type Id '[' MNum ']' ';'              {$$ = MDecl $1 $2 $4;
                                                             $$.position = $1.position;
                                                             $$.stable = if member (fromId $2) $ head $$.itable
                                                                         then $$.itable
                                                                         else if $1 == TypeInt
                                                                              then (insert (fromId $2) (TypeArray TypeInt) $ head $$.itable):(tail $$.itable)
                                                                              else (insert (fromId $2) (TypeArray TypeChar) $ head $$.itable):(tail $$.itable);
                                                             $$.errors = if member (fromId $2) $ head $$.itable
                                                                         then [(show$lineNumber $2.position)++":"++(show$colNumber $2.position)++": error: symbol '"++(fromId $2)++"' redeclared"]
                                                                         else []}
Type                : int                                   {$$ = TypeInt; $$.position = $1}
                    | char                                  {$$ = TypeChar; $$.position = $1}
StmtList            : Stmt                                  {$$ = [$1];
                                                             $1.itable = $$.itable;
                                                             $$.errors = $1.errors;
                                                             $1.incycle = $$.incycle}
                    | Stmt StmtList                         {$$ = $1:$2;
                                                             $1.itable = $$.itable;
                                                             $2.itable = $$.itable;
                                                             $$.errors = $1.errors ++ $2.errors;
                                                             $1.incycle = $$.incycle;
                                                             $2.incycle = $$.incycle}
Stmt                : ';'                                   {$$ = Nop; $$.errors = []}
                    | Expr ';'                              {$$ = Stmt $1; $1.itable = $$.itable; $$.errors = $1.errors}
                    | Expr error                            {$$ = Stmt $1; $1.itable = $$.itable; $$.errors = $1.errors ++ [(show$lineNumber $1.position)++":"++(show$colNumber $1.position)++": error: semicolon missed"]}
                    | return Expr ';'                       {$$ = Ret $2; 
                                                             $2.itable = $$.itable; 
                                                             $$.errors = case $2.atype of
                                                                        { TypeInt -> [];
                                                                          TypeChar -> [];
                                                                          _ -> [(show$lineNumber  $2.position)++":"++(show$colNumber  $2.position)++": error: operand should be of integral type"]
                                                                         } ++ $2.errors}
                    
                    | return Expr error                     {$$ = Ret $2; 
                                                             $2.itable = $$.itable; 
                                                             $$.errors = case $2.atype of
                                                                        { TypeInt -> [];
                                                                          TypeChar -> [];
                                                                          _ -> [(show$lineNumber  $2.position)++":"++(show$colNumber  $2.position)++": error: operand should be of integral type"]
                                                                         } ++ $2.errors ++ [(show$lineNumber $2.position)++":"++(show$colNumber $2.position)++": error: semicolon missed"]}
                    
                    | read Id ';'                           {$$ = if $$.mtype == Just TypeInt
                                                                  then Read $2 TypeInt
                                                                  else Read $2 TypeChar;
                                                             $$.mtype = lookup (fromId $2) $$.itable;
                                                             $$.errors = case $$.mtype of
                                                                        {Nothing -> [(show$lineNumber $2.position)++":"++(show$colNumber $2.position)++": error: '"++(fromId $2)++"' undeclared"];
                                                                         Just TypeInt -> [];
                                                                         Just TypeChar -> [];
                                                                         _ -> [(show$lineNumber $2.position)++":"++(show$colNumber $2.position)++": error: '"++(fromId $2)++"' is not integral type"]
                                                                        }
                                                             }
                    | read Id error                          {$$ = if $$.mtype == Just TypeInt
                                                                  then Read $2 TypeInt
                                                                  else Read $2 TypeChar;
                                                             $$.mtype = lookup (fromId $2) $$.itable;
                                                             $$.errors = case $$.mtype of
                                                                        {Nothing -> [(show$lineNumber $2.position)++":"++(show$colNumber $2.position)++": error: '"++(fromId $2)++"' undeclared"];
                                                                         Just TypeInt -> [];
                                                                         Just TypeChar -> [];
                                                                         _ -> [(show$lineNumber $2.position)++":"++(show$colNumber $2.position)++": error: '"++(fromId $2)++"' is not integral type"]
                                                                        } ++ [(show$lineNumber $2.position)++":"++(show$colNumber $2.position)++": error: semicolon missed"]
                                                             }
                    
                    | write Expr ';'                        {$$ = Write $2; 
                                                             $2.itable = $$.itable; 
                                                             $$.errors = case $2.atype of
                                                                        { TypeInt -> [];
                                                                          TypeChar -> [];
                                                                          _ -> [(show$lineNumber  $2.position)++":"++(show$colNumber  $2.position)++": error: operand should be of integral type"]
                                                                         } ++ $2.errors}
                    | write Expr error                      {$$ = Write $2; 
                                                             $2.itable = $$.itable; 
                                                             $$.errors = case $2.atype of
                                                                        { TypeInt -> [];
                                                                          TypeChar -> [];
                                                                          _ -> [(show$lineNumber  $2.position)++":"++(show$colNumber  $2.position)++": error: operand should be of integral type"]
                                                                         } ++ $2.errors ++ [(show$lineNumber $2.position)++":"++(show$colNumber $2.position)++": error: semicolon missed"]}
                    | writeln ';'                           {$$ = WriteLn; $$.errors = []}
                    | writeln error                         {$$ = WriteLn; $$.errors = [(show$lineNumber $1)++":"++(show$colNumber $1)++": error: semicolon missed"]}
                    | break ';'                             {$$ = Break; $$.errors = if $$.incycle
                                                                                     then []
                                                                                     else [(show$lineNumber $1)++":"++(show$colNumber $1)++": error: break statement is not in a cycle"]} 
                    | break error                           {$$ = Break; $$.errors = (if $$.incycle
                                                                                      then []
                                                                                      else [(show$lineNumber $1)++":"++(show$colNumber $1)++": error: break statement is not in a cycle"]) ++
                                                                                      [(show$lineNumber $1)++":"++(show$colNumber $1)++": error: semicolon missed"]} 
                    | if '(' Expr ')' Stmt else Stmt        {$$ = IfElse $3 $5 $7;
                                                             $$.position = $1;
                                                             $3.itable = $$.itable;
                                                             $5.itable = $$.itable;
                                                             $7.itable = $$.itable;
                                                             $5.incycle = $$.incycle;
                                                             $7.incycle = $$.incycle;
                                                             $$.errors = case $3.atype of
                                                                        { TypeInt -> [];
                                                                          TypeChar -> [];
                                                                          _ -> [(show$lineNumber  $3.position)++":"++(show$colNumber  $3.position)++": error: operand should be of integral type"]
                                                                         } ++ $3.errors ++ $5.errors ++ $7.errors}
                    | while '(' Expr ')' Stmt               {$$ = While $3 $5;
                                                             $$.position = $1;
                                                             $3.itable = $$.itable;
                                                             $5.itable = $$.itable;
                                                             $5.incycle = True;
                                                             $$.errors = case $3.atype of
                                                                        { TypeInt -> [];
                                                                          TypeChar -> [];
                                                                          _ -> [(show$lineNumber  $3.position)++":"++(show$colNumber  $3.position)++": error: operand should be of integral type"]
                                                                         } ++ $3.errors ++ $5.errors}
                    | Block                                 {$$ = $1; $1.incycle = $$.incycle; $$.position = $1.position; $1.itable = $$.itable; $$.errors = $1.errors} 

Expr                : Primary                               {$$ = $1; $$.position = $1.position; $$.atype = $1.atype; $1.itable = $$.itable; $$.errors = $1.errors}
                    | '-' Expr  %prec NEG                   {$$ = Negate $2 $$.atype; 
                                                             $$.position = $1; 
                                                             $$.atype = TypeInt; 
                                                             $2.itable = $$.itable; 
                                                             $$.errors = case $2.atype of
                                                                        { TypeInt -> [];
                                                                          TypeChar -> [];
                                                                          _ -> [(show$lineNumber  $2.position)++":"++(show$colNumber  $2.position)++": error: operand should be of integral type"]
                                                                         } ++ $2.errors}
                    | '!' Expr                              {$$ = Not $2 $$.atype; 
                                                             $$.position = $1; 
                                                             $$.atype = $2.atype; 
                                                             $2.itable = $$.itable; 
                                                             $$.errors = case $2.atype of
                                                                        { TypeInt -> [];
                                                                          TypeChar -> [];
                                                                          _ -> [(show$lineNumber  $2.position)++":"++(show$colNumber  $2.position)++": error: operand should be of integral type"]
                                                                         } ++ $2.errors}
                    | Expr '*' Expr                         {$$.atype = liftType $1.atype $3.atype;$$.position = $1.position;$$ = Mult $1 $3 $$.atype;$1.itable = $$.itable;$3.itable = $$.itable; $$.errors = (binOpTypeErrors $1.atype $1.position $3.atype $3.position) ++ $1.errors ++ $3.errors}
                    | Expr '/' Expr                         {$$.atype = liftType $1.atype $3.atype;$$.position = $1.position;$$ = Div $1 $3 $$.atype;$1.itable = $$.itable;$3.itable = $$.itable; $$.errors = (binOpTypeErrors $1.atype $1.position $3.atype $3.position) ++ $1.errors ++ $3.errors}
                    | Expr '+' Expr                         {$$.atype = liftType $1.atype $3.atype;$$.position = $1.position;$$ = Plus $1 $3 $$.atype;$1.itable = $$.itable;$3.itable = $$.itable; $$.errors = (binOpTypeErrors $1.atype $1.position $3.atype $3.position) ++ $1.errors ++ $3.errors}
                    | Expr '-' Expr                         {$$.atype = liftType $1.atype $3.atype;$$.position = $1.position;$$ = Minus $1 $3 $$.atype;$1.itable = $$.itable;$3.itable = $$.itable; $$.errors = (binOpTypeErrors $1.atype $1.position $3.atype $3.position) ++ $1.errors ++ $3.errors}
                    | Expr '&&' Expr                        {$$.atype = liftType $1.atype $3.atype;$$.position = $1.position;$$ = And $1 $3 $$.atype;$1.itable = $$.itable;$3.itable = $$.itable; $$.errors = (binOpTypeErrors $1.atype $1.position $3.atype $3.position) ++ $1.errors ++ $3.errors}
                    | Expr '||' Expr                        {$$.atype = liftType $1.atype $3.atype;$$.position = $1.position;$$ = Or $1 $3 $$.atype;$1.itable = $$.itable;$3.itable = $$.itable; $$.errors = (binOpTypeErrors $1.atype $1.position $3.atype $3.position) ++ $1.errors ++ $3.errors}
                    | Expr '>' Expr                         {$$.atype = liftType $1.atype $3.atype;$$.position = $1.position;$$ = Greater $1 $3 $$.atype;$1.itable = $$.itable;$3.itable = $$.itable; $$.errors = (binOpTypeErrors $1.atype $1.position $3.atype $3.position) ++ $1.errors ++ $3.errors}
                    | Expr '<' Expr                         {$$.atype = liftType $1.atype $3.atype;$$.position = $1.position;$$ = Less $1 $3 $$.atype;$1.itable = $$.itable;$3.itable = $$.itable; $$.errors = (binOpTypeErrors $1.atype $1.position $3.atype $3.position) ++ $1.errors ++ $3.errors}
                    | Expr '>=' Expr                        {$$.atype = liftType $1.atype $3.atype;$$.position = $1.position;$$ = GEq $1 $3 $$.atype;$1.itable = $$.itable;$3.itable = $$.itable; $$.errors = (binOpTypeErrors $1.atype $1.position $3.atype $3.position) ++ $1.errors ++ $3.errors}
                    | Expr '<=' Expr                        {$$.atype = liftType $1.atype $3.atype;$$.position = $1.position;$$ = LEq $1 $3 $$.atype;$1.itable = $$.itable;$3.itable = $$.itable; $$.errors = (binOpTypeErrors $1.atype $1.position $3.atype $3.position) ++ $1.errors ++ $3.errors}
                    | Expr '==' Expr                        {$$.atype = liftType $1.atype $3.atype;$$.position = $1.position;$$ = Eq $1 $3 $$.atype;$1.itable = $$.itable;$3.itable = $$.itable; $$.errors = (binOpTypeErrors $1.atype $1.position $3.atype $3.position) ++ $1.errors ++ $3.errors}
                    | Expr '!=' Expr                        {$$.atype = liftType $1.atype $3.atype;$$.position = $1.position;$$ = NEq $1 $3 $$.atype;$1.itable = $$.itable;$3.itable = $$.itable; $$.errors = (binOpTypeErrors $1.atype $1.position $3.atype $3.position) ++ $1.errors ++ $3.errors}
                    | Id '[' Expr ']' '=' Expr              {$$.mtype = lookup (fromId $1) $$.itable;
                                                             $$.atype = maybe TypeInt retType $$.mtype;
                                                             $$ = MAssign $1 $3 $6 $$.atype;
                                                             $$.position = $1.position;
                                                             $3.itable = $$.itable;
                                                             $6.itable = $$.itable;
                                                             $$.errors = case $$.mtype of
                                                                        { Nothing -> [(show$lineNumber  $1.position)++":"++(show$colNumber $1.position)++": error: '"++(fromId $1)++"' undeclared"];
                                                                          Just (TypeArray TypeInt) -> [];
                                                                          Just (TypeArray TypeChar) -> [];
                                                                          _ -> [(show$lineNumber $1.position)++":"++(show$colNumber $1.position)++": error: '"++(fromId $1)++"' is not array"]
                                                                         } ++ 
                                                                         case $3.atype of
                                                                         { TypeInt -> [];
                                                                           TypeChar -> [];
                                                                           _ -> [(show$lineNumber  $3.position)++":"++(show$colNumber  $3.position)++": error: array subscript is not an integer"]
                                                                         } ++
                                                                         case $6.atype of
                                                                          { TypeInt -> [];
                                                                            TypeChar -> [];
                                                                            _ -> [(show$lineNumber  $6.position)++":"++(show$colNumber  $6.position)++": error: trying to assign function or array"]
                                                                          } ++ $3.errors ++ $6.errors
                                                            }
                                                                         
                    | Id '=' Expr                           {$$.mtype = lookup (fromId $1) $$.itable;
                                                             $$.atype = maybe TypeInt retType $$.mtype;
                                                             $$ = Assign $1 $3 $$.atype;
                                                             $$.position = $1.position;
                                                             $3.itable = $$.itable;
                                                             $$.errors = case $$.mtype of
                                                                         { Nothing -> [(show$lineNumber $1.position)++":"++(show$colNumber $1.position)++": error: '"++(fromId $1)++"' undeclared"];
                                                                           Just TypeInt -> [];
                                                                           Just TypeChar -> [];
                                                                           _ -> [(show$lineNumber $1.position)++":"++(show$colNumber $1.position)++": error: '"++(fromId $1)++"' is not of integral type"]
                                                                          } ++
                                                                          case $3.atype of
                                                                          { TypeInt -> [];
                                                                            TypeChar -> [];
                                                                            _ -> [(show$lineNumber $3.position)++":"++(show$colNumber $3.position)++": error: trying to assign function or array"]
                                                                          }++ $3.errors
                                                            }
                                                                         
                    
Primary             : Id                                    {$$.mtype = lookup (fromId $1) $$.itable;
                                                             $$ = IExpr (fromId $1) $$.atype;
                                                             $$.position = $1.position;
                                                             $$.atype = maybe TypeInt id $$.mtype;
                                                             $$.errors = case $$.mtype of
                                                                         { Nothing -> [(show$lineNumber  $1.position)++":"++(show$colNumber $1.position)++": error: '"++(fromId $1)++"' undeclared"];
                                                                           _ -> []
                                                                          }
                                                            }
                                                                         
                    | MNum                                  {$$ = NExpr (fromMNum $1) $1.atype; $$.position = $1.position;$$.atype = $1.atype; $$.errors = []}
                    | '(' Expr ')'                          {$$ = $2; $$.position = $1; $2.itable = $$.itable;$$.errors = $2.errors;$$.atype = $2.atype}
                    | '(' Expr error                        {$$ = $2; $$.position = $1; $2.itable = $$.itable;$$.errors = [(show$lineNumber $2.position) ++ ":" ++ (show$lineNumber $2.position) ++ ": error: right parathesis missed"] ++ $2.errors;$$.atype = $2.atype}
                    | Id '(' ExprList ')'                   {$$.mtype = lookup (fromId $1) $$.itable;
                                                             $$.atype = maybe TypeInt retType $$.mtype;
                                                             $$ = Func  $1 $3 $$.atype; 
                                                             $$.position = $1.position;
                                                             $3.itable = $$.itable;
                                                             $$.errors = case $$.mtype of 
                                                                         {Nothing -> [(show$lineNumber $1.position)++":"++(show$colNumber $1.position)++": error: '"++(fromId $1)++"' undeclared"];
                                                                          Just (TypeFunction _ args) -> if args == (map exprType $3) 
                                                                                                       then []
                                                                                                       else [(show$lineNumber $1.position)++":"++(show$colNumber $1.position)++": error: wrong function signature"];                                                                          
                                                                          _ -> [(show$lineNumber $1.position)++":"++(show$colNumber $1.position)++": error: '"++(fromId $1)++"' is not a function"]
                                                                          } ++ $3.errors
                                                            }
                    | Id '[' Expr ']'                       {$$.mtype = lookup (fromId $1) $$.itable;
                                                             $$.atype = maybe TypeInt retType $$.mtype;
                                                             $$ = M $1 $3 $$.atype;
                                                             $$.position = $1.position;
                                                             $3.itable = $$.itable;
                                                             $$.errors  =  case $$.mtype of
                                                                           { Nothing -> [(show$lineNumber $1.position)++":"++(show$colNumber $1.position)++": error: '"++(fromId $1)++"' undeclared"];
                                                                             Just (TypeArray TypeInt) -> [];
                                                                             Just (TypeArray TypeChar) -> [];
                                                                             _ -> [(show$lineNumber $1.position)++":"++(show$colNumber $1.position)++": error: '"++(fromId $1)++"' is not array"]
                                                                            } ++
                                                                            case $3.atype of
                                                                            {TypeInt -> [];
                                                                             TypeChar -> [];
                                                                             _ -> [(show$lineNumber $3.position)++":"++(show$colNumber $3.position)++": error: array subscript is not an integer"]
                                                                            } ++ $3.errors
                                                             }
ExprList            : {- empty -}                           {$$ = []; $$.errors = []}
                    | ExprListTail                          {$$ = $1; $$.position = $1.position; $1.itable = $$.itable; $$.errors = $1.errors}
ExprListTail        : Expr                                  {$$ = [$1]; $$.position = $1.position; $1.itable = $$.itable; $$.errors = $1.errors}
                    | Expr ',' ExprListTail                 {$$ = $1:$3; $$.position = $1.position; $1.itable = $$.itable;$3.itable = $$.itable;$$.errors = $1.errors ++ $3.errors}
Id                  : id                                    {$$ = Id (getId $1); $$.position = getAPN $1}
MNum                : numc                                  {$$ = MNum (getNum $1) TypeInt; $$.position = getAPN $1; $$.atype = TypeInt}
                    | charc                                 {$$ = MNum (getNum $1) TypeChar; $$.position = getAPN $1; $$.atype = TypeChar}

{


getNum (TNumConst a _ ) = a
getNum (TCharConst a _ ) = a
getId  (TId a _ )  = a
getAPN (TNumConst _ b ) = b
getAPN (TCharConst _ b ) = b
getAPN (TId _ b )  = b
    


parseError :: [Token] -> a
parseError (t:ts)  = error ("Parse error before "++(show t))

--data IdType = IdChar | IdInt | IdCharArray | IdIntArray | IdIntFunction [IdType] | IdCharFunction [IdType] deriving (Eq,Show)


--fromIdType :: IdType -> Type
--fromIdType IdChar = TypeChar
--fromIdType IdCharArray = TypeChar
--fromIdType IdCharFunction = TypeChar
--fromIdType _ = TypeInt 

type SymTable = Map String Type

lookup ::  String ->[SymTable]-> Maybe Type
lookup  _ []= Nothing
lookup s (t:ts) = f $ Map.lookup s t
    where f Nothing = lookup s ts
          f x = x

pDecl2Type (ParamVarDecl t _ ) = t
pDecl2Type (ParamMDecl t _ )   = TypeArray t

data Id = Id String deriving (Eq,Show)
fromId (Id s ) = s

data MNum = MNum Int Type deriving (Eq,Show)
fromMNum (MNum a _ ) = a
typefromMNum (MNum _ t) = t 

data Expr = IExpr String Type           |
            NExpr Int Type              |
            Func Id [Expr] Type         |
            M Id Expr Type              |
            Negate Expr Type            |
            Not Expr Type               |
            Mult Expr Expr Type         |
            Div Expr Expr Type          |
            Plus Expr Expr Type         |
            Minus Expr Expr Type        |
            And Expr Expr Type          |
            Or Expr Expr Type           |
            Greater Expr Expr Type      |
            Less Expr Expr Type         |
            GEq Expr Expr Type          |
            LEq Expr Expr Type          |
            Eq Expr Expr Type           |
            NEq Expr Expr Type          |
            Assign Id Expr Type         |
            MAssign Id Expr Expr Type
            deriving (Eq,Show)
            
exprType (IExpr _ t) = t
exprType (NExpr _ t) = t
exprType (Func _ _ t) = t
exprType (M _ _ t) = t
exprType (Negate _ t) = t
exprType (Not _ t) = t
exprType (Mult _ _ t) = t
exprType (Div _ _ t) = t
exprType (Plus _ _ t) = t
exprType (Minus _ _ t) = t
exprType (And _ _ t) = t
exprType (Or _ _ t) = t
exprType (Greater _ _ t) = t
exprType (Less _ _ t) = t
exprType (GEq _ _ t) = t
exprType (LEq _ _ t) = t
exprType (Eq _ _ t) = t
exprType (NEq _ _ t) = t
exprType (Assign _ _ t) = t
exprType (MAssign _ _ _ t) = t
            
type ExprList = [Expr]-- deriving (Eq,Show)
data Stmt = Nop|
            Stmt Expr|
            Block VarDeclList StmtList|
            Ret Expr|
            Read Id Type|
            Write Expr|
            WriteLn|
            Break|
            IfElse Expr Stmt Stmt|
            While Expr Stmt
            deriving (Eq,Show)
type StmtList = [Stmt]            
data Type = TypeInt | TypeChar | TypeArray Type | TypeFunction Type [Type] deriving (Eq,Show)

retType (TypeArray t) = t
retType (TypeFunction t _) = t
retType t = t

liftType :: Type -> Type -> Type
liftType a b = if a==TypeInt || b==TypeInt
               then TypeInt
               else TypeChar    

binOpTypeErrors t1 p1 t2 p2  = case t1 of
                               { TypeInt -> [];
                                 TypeChar -> [];
                                 _ -> [(show$lineNumber p1) ++ ":" ++ (show$colNumber p1) ++ ": error: operand should be of integral type" ]
                                } ++
                                case t2 of
                               { TypeInt -> [];
                                 TypeChar -> [];
                                 _ -> [(show$lineNumber p2) ++ ":" ++ (show$colNumber p2) ++ ": error: operand should be of integral type" ]
                                }
               
--data VarDecl = VarDecl Type Id| MDecl Type Id MNum deriving (Eq,Show)
--type VarDeclList = [VarDecl]    
--data FuncDecl = FunDecl Type Id ParamDeclList Stmt deriving  (Eq,Show)
--type FuncDeclList = [FuncDecl]
data ParamDecl = ParamVarDecl Type Id | ParamMDecl Type Id deriving (Eq,Show)
type ParamDeclList = [ParamDecl] 
type VarDeclList = [Decl] 
--data Program = Program VarDeclList FuncDeclList 

data Decl = VarDecl Type Id| MDecl Type Id MNum|FunDecl Type Id ParamDeclList Stmt deriving (Eq,Show)

data Program = Ok [Decl] | Error [String] deriving (Eq,Show)
}           