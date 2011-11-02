{
module Parser where
import Lex
}

%name parse DeclList
%name parseExpr Expr
%tokentype { Token }
%error { parseError}

%token
        int {TInt a}
        char {TChar a}
        if {TIf a}
        else {TElse a}
        while {TWhile a}
        write {TWrite a}
        writeln {TWriteLn a}
        read {TRead a}
        return {TReturn a}
        break {TBreak a}
        num {TNum $$ b }
        id {TId $$ b}
        ',' {TComma a}
--        '.' {TDot a}
        ';' {TSemicolon a}
        '(' {TLPar a }
        ')' {TRPar a }
        '[' {TLBracket a}
        ']' {TRBracket a}
        '{' {TLCurly a}
        '}' {TRCurly a}
        '=' {TAssign a}
        '!' {TNot a}
        '-' {TMinus a}
        '+' {TPlus a}
        '*' {TMult a}
        '/' {TDiv a}
        '!=' {TNEq a}
        '<' {TLess a}
        '>' {TGreater a}
        '<=' {TLEq a}
        '>=' {TGEq a}
        '&&' {TAnd a}
        '||' {TOr a}
        '==' {TEq a}
        
%right '='
%left '||'
%left '&&'
%left '==' '!='
%left '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%right NEG '!'

%%
--Program             : DeclList
DeclList            : Decl                                  {[$1]}
                    | Decl DeclList                         {$1:$2}
Decl                : VarDecl                               {$1}
                    | FunDecl                               {$1}
FunDecl             : Type Id '(' ParamDeclList ')'Block    {FunDecl $1 $2 $4 $6}
ParamDeclList       : {-empty-}                             {[]}
                    | ParamDeclListTail                     {$1}
ParamDeclListTail   : ParamDecl                             {[$1]}
                    | ParamDecl ',' ParamDeclListTail       {$1:$3}
ParamDecl           : Type Id                               {ParamVarDecl $1 $2}
                    | Type Id '[' ']'                       {ParamMDecl $1 $2}
VarDeclList         : {-empty-}                             {[]}
                    | VarDeclListTail                       {$1}
VarDeclListTail     : VarDecl                               {[$1]}
                    | VarDecl VarDeclListTail               {$1:$2}
Block               : '{' VarDeclList StmtList '}'          {Block $2 $3}
VarDecl             : Type Id ';'                           {VarDecl $1 $2}
                    | Type Id '[' MNum ']' ';'              {MDecl $1 $2 $4}
Type                : int                                   {TypeInt}
                    | char                                  {TypeChar}
StmtList            : Stmt                                  {[$1]}
                    | Stmt StmtList                         {$1:$2}
Stmt                : ';'                                   {Nop}
                    | Expr ';'                              {Stmt $1}
                    | return Expr ';'                       {Ret $2}
                    | read Id ';'                           {Read $2}
                    | write Expr ';'                        {Write $2}
                    | writeln ';'                           {WriteLn}
                    | break ';'                             {Break}
                    | if '(' Expr ')' Stmt else Stmt        {IfElse $3 $5 $7}
                    | while '(' Expr ')' Stmt               {While $3 $5}
                    | Block                                 {$1} 

Expr                : Primary                               {$1}
                    | '-' Expr  %prec NEG                   {Negate $2}
                    | '!' Expr                              {Not $2}
                    | Expr '*' Expr                         {Mult $1 $3}
                    | Expr '/' Expr                         {Div $1 $3}
                    | Expr '+' Expr                         {Plus $1 $3}
                    | Expr '-' Expr                         {Minus $1 $3}
                    | Expr '&&' Expr                        {And $1 $3}
                    | Expr '||' Expr                        {Or $1 $3}
                    | Expr '>' Expr                         {Greater $1 $3}
                    | Expr '<' Expr                         {Less $1 $3}
                    | Expr '>=' Expr                        {GEq $1 $3}
                    | Expr '<=' Expr                        {LEq $1 $3}
                    | Expr '==' Expr                        {Eq $1 $3}
                    | Expr '!=' Expr                        {NEq $1 $3}
                    --| Expr BinOp Expr                       {BinExpr $1 $2 $2}
                    | Id '[' Expr ']' '=' Expr              {MAssign $1 $3 $6}
                    | Id '=' Expr                           {Assign $1 $3}
                    
Primary             : Id                                    {IExpr $1}
                    | MNum                                  {NExpr $1}
                    | '(' Expr ')'                          {$2}
                    | Id '(' ExprList ')'                   {Func  $1 $3}
                    | Id '[' Expr ']'                       {M $1 $3}
ExprList            : {- empty -}                           {[]}
                    | ExprListTail                          {$1}
ExprListTail        : Expr                                  {[$1]}
                    | Expr ',' ExprListTail                 {$1:$3}
Id                  : id                                    {Id $1}
MNum                : num                                   {MNum $1}                    

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Id = Id String deriving (Eq,Show)
data MNum = MNum Int deriving (Eq,Show)
data Expr = IExpr Id                |
            NExpr MNum              |
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

}           