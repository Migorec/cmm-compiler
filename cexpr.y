{
module Parser where
import Lex
import Data.Map hiding(map)
}

%attributetype {MyAttr a}
%attribute value { a }
%attribute errors {[String]}
%attribute itable { [SymTable]} -- Наследуемая таблица символов
%attribute stable { [SymTable]} -- Синтезируемая таблица символов

%name parse Program
%name parseExpr Expr
%tokentype { Token }
%error { parseError}

%token
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
        num {TNum a b }
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
Program             : DeclList                              {$$ = if $$.errors == [] then Ok $1 else Error $$.errors; 
                                                             $$.stable = $1.stable;
                                                             $1.itable = [empty];
                                                             $$.errors = $1.errors}
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
                                                                         else (insert (fromId $2) (IdFunction (map pDecl2IdType $4)) $ head $$.itable):(tail $$.itable);
                                                             $$.errors = if member (fromId $2) $ head $$.itable
                                                                         then [(show$lineNumber$pnfromId $2)++":"++(show$colNumber$pnfromId $2)++": error: symbol '"++(fromId $2)++"' redeclared"] ++ $4.errors ++ $6.errors
                                                                         else [] ++ $4.errors ++ $6.errors;
                                                             $4.itable = [empty];
                                                             $6.itable = $4.stable ++ $$.stable
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
                                                                         else (insert (fromId $2) IdSingle $ head $$.itable):(tail $$.itable);
                                                             $$.errors = if member (fromId $2) $ head $$.itable
                                                                         then [(show$lineNumber$pnfromId $2)++":"++(show$colNumber$pnfromId $2)++": error: redifinition of parametr '"++(fromId $2)++"'"]
                                                                         else []}
                    | Type Id '[' ']'                       {$$ = ParamMDecl $1 $2;
                                                             $$.stable = if member (fromId $2) $ head $$.itable
                                                                         then $$.itable
                                                                         else (insert (fromId $2) IdArray $ head $$.itable):(tail $$.itable);
                                                             $$.errors = if member (fromId $2) $ head $$.itable
                                                                         then [(show$lineNumber$pnfromId $2)++":"++(show$colNumber$pnfromId $2)++": error: redifinition of parametr '"++(fromId $2)++"'"]
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
                                                             $$.errors = $2.errors }
VarDecl             : Type Id ';'                           {$$ = VarDecl $1 $2;
                                                             $$.stable = if  member (fromId $2) $ head $$.itable
                                                                         then $$.itable
                                                                         else (insert (fromId $2) IdSingle $ head $$.itable):(tail $$.itable);
                                                             $$.errors = if member (fromId $2) $ head $$.itable
                                                                         then [(show$lineNumber$pnfromId $2)++":"++(show$colNumber$pnfromId $2)++": error: symbol '"++(fromId $2)++"' redeclared"]
                                                                         else []}
                    | Type Id '[' MNum ']' ';'              {$$ = MDecl $1 $2 $4;
                                                             $$.stable = if member (fromId $2) $ head $$.itable
                                                                         then $$.itable
                                                                         else (insert (fromId $2) IdArray $ head $$.itable):(tail $$.itable);
                                                             $$.errors = if member (fromId $2) $ head $$.itable
                                                                         then [(show$lineNumber$pnfromId $2)++":"++(show$colNumber$pnfromId $2)++": error: symbol '"++(fromId $2)++"' redeclared"]
                                                                         else []}
Type                : int                                   {$$ = TypeInt}
                    | char                                  {$$ = TypeChar}
StmtList            : Stmt                                  {$$ = [$1]}
                    | Stmt StmtList                         {$$ = $1:$2}
Stmt                : ';'                                   {$$ = Nop}
                    | Expr ';'                              {$$ = Stmt $1}
                    | return Expr ';'                       {$$ = Ret $2}
                    | read Id ';'                           {$$ = Read $2}
                    | write Expr ';'                        {$$ = Write $2}
                    | writeln ';'                           {$$ = WriteLn}
                    | break ';'                             {$$ = Break}
                    | if '(' Expr ')' Stmt else Stmt        {$$ = IfElse $3 $5 $7}
                    | while '(' Expr ')' Stmt               {$$ = While $3 $5}
                    | Block                                 {$$ = $1} 

Expr                : Primary                               {$$ = $1}
                    | '-' Expr  %prec NEG                   {$$ = Negate $2}
                    | '!' Expr                              {$$ = Not $2}
                    | Expr '*' Expr                         {$$ = Mult $1 $3}
                    | Expr '/' Expr                         {$$ = Div $1 $3}
                    | Expr '+' Expr                         {$$ = Plus $1 $3}
                    | Expr '-' Expr                         {$$ = Minus $1 $3}
                    | Expr '&&' Expr                        {$$ = And $1 $3}
                    | Expr '||' Expr                        {$$ = Or $1 $3}
                    | Expr '>' Expr                         {$$ = Greater $1 $3}
                    | Expr '<' Expr                         {$$ = Less $1 $3}
                    | Expr '>=' Expr                        {$$ = GEq $1 $3}
                    | Expr '<=' Expr                        {$$ = LEq $1 $3}
                    | Expr '==' Expr                        {$$ = Eq $1 $3}
                    | Expr '!=' Expr                        {$$ = NEq $1 $3}
                    | Id '[' Expr ']' '=' Expr              {$$ = MAssign $1 $3 $6}
                    | Id '=' Expr                           {$$ = Assign $1 $3}
                    
Primary             : Id                                    {$$ = IExpr (fromId $1)}
                    | MNum                                  {$$ = NExpr (fromMNum $1)}
                    | '(' Expr ')'                          {$$ = $2}
                    | Id '(' ExprList ')'                   {$$ = Func  $1 $3}
                    | Id '[' Expr ']'                       {$$ = M $1 $3}
ExprList            : {- empty -}                           {$$ = []}
                    | ExprListTail                          {$$ = $1}
ExprListTail        : Expr                                  {$$ = [$1]}
                    | Expr ',' ExprListTail                 {$$ = $1:$3}
Id                  : id                                    {$$ = Id (getId $1) (getAPN $1)}
MNum                : num                                   {$$ = MNum (getNum $1) (getAPN $1)}                    

{


getNum (TNum a _ ) = a
getId  (TId a _ )  = a
getAPN (TNum _ b ) = b
getAPN (TId _ b )  = b
    


parseError :: [Token] -> a
parseError _  = error "Parse error" 

data IdType = IdSingle | IdArray | IdFunction [IdType] deriving (Eq,Show)

type SymTable = Map String IdType

pDecl2IdType (ParamVarDecl _ _ ) = IdSingle
pDecl2IdType (ParamMDecl _ _ )   = IdArray

data Id = Id String AlexPosn deriving (Eq,Show)
fromId (Id s _ ) = s
pnfromId (Id _ p) = p

data MNum = MNum Int AlexPosn deriving (Eq,Show)
fromMNum (MNum a _ ) = a
pnfromMNum (MNum _ p) = p 

data Expr = IExpr String            |
            NExpr Int               |
            Func Id [Expr]          |
            M Id Expr               |
            Negate Expr             |
            Not Expr                |
            Mult Expr Expr          |
            Div Expr Expr           |
            Plus Expr Expr          |
            Minus Expr Expr         |
            And Expr Expr           |
            Or Expr Expr            |
            Greater Expr Expr       |
            Less Expr Expr          |
            GEq Expr Expr           |
            LEq Expr Expr           |
            Eq Expr Expr            |
            NEq Expr Expr           |
            Assign Id Expr          |
            MAssign Id Expr Expr
            deriving (Eq,Show)
type ExprList = [Expr]-- deriving (Eq,Show)
data Stmt = Nop|
            Stmt Expr|
            Block VarDeclList StmtList|
            Ret Expr|
            Read Id|
            Write Expr|
            WriteLn|
            Break|
            IfElse Expr Stmt Stmt|
            While Expr Stmt
            deriving (Eq,Show)
type StmtList = [Stmt]            
data Type = TypeInt|TypeChar deriving (Eq,Show)

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