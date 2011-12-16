{
module Lex (scan,Token(..),AlexPosn(..),lineNumber,colNumber) where

}
%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-z]

tokens :-
    
    -- пробелы, пустые строки и т.п.
    $white+                                         ;
    -- коментарии
    "//".*                                          ;
    "/*"[$white $printable]*"*/"                    ;
    -- типы
    int                                             { \p s -> TInt p}
    char                                            { \p s -> TChar p}
    -- операторы
    if                                              { \p s -> TIf p}
    else                                            { \p s -> TElse p}
    write                                           { \p s -> TWrite p}
    writeln                                         { \p s -> TWriteLn p}
    return                                          { \p s -> TReturn p}
    break                                           { \p s -> TBreak p}
    read                                            { \p s -> TRead p}
    while                                           { \p s -> TWhile p}
    -- константы
    "'"$alpha"'"                                    { \p s -> TNum (ord$ read s) p}
    $digit+                                         { \p s -> TNum (read s) p}
    
    -- разделители
  --  "."                                             { \p s -> TDot p}
    ","                                             { \p s -> TComma p}
    ";"                                             { \p s -> TSemicolon p}
    "("                                             { \p s -> TLPar p}
    ")"                                             { \p s -> TRPar p}
    "["                                             { \p s -> TLBracket p}
    "]"                                             { \p s -> TRBracket p}
    "{"                                             { \p s -> TLCurly p}
    "}"                                             { \p s -> TRCurly p}
    -- операции
    "="                                             { \p s -> TAssign p}
    "!"                                             { \p s -> TNot p}
    "-"                                             { \p s -> TMinus p}
    "+"                                             { \p s -> TPlus p}
    "*"                                             { \p s -> TMult p}
    "/"                                             { \p s -> TDiv p}
    "=="                                            { \p s -> TEq p}
    "!="                                            { \p s -> TNEq p}
    "<"                                             { \p s -> TLess p}
    ">"                                             { \p s -> TGreater p}
    "<="                                            { \p s -> TLEq p}
    ">="                                            { \p s -> TGEq p}
    "&&"                                            { \p s -> TAnd p}
    "||"                                            { \p s -> TOr p}
    -- идентефикатор
    [a-zA-Z\_][a-zA-Z\_0-9]*                         { \p s -> TId s p}
    --"[$alpha \_] [$alpha $digit \_ ]*"                { \p s -> TId s p}
{
data Token = TInt AlexPosn      |
             TChar AlexPosn     |
             TIf  AlexPosn      |
             TElse  AlexPosn    |
             TWhile AlexPosn    |
             TWrite AlexPosn    |
             TWriteLn AlexPosn  |
             TRead AlexPosn     |
             TReturn AlexPosn   |
             TBreak AlexPosn    |
             TNum Int AlexPosn  |
             TId String AlexPosn|
             TComma AlexPosn    |
             TDot AlexPosn      |
             TSemicolon AlexPosn|
             TLPar AlexPosn     |
             TRPar AlexPosn     |
             TLBracket AlexPosn |
             TRBracket AlexPosn |
             TLCurly AlexPosn   |
             TRCurly AlexPosn   |
             TAssign AlexPosn   |
             TNot AlexPosn      |
             TMinus AlexPosn    |
             TPlus AlexPosn     |
             TMult AlexPosn     |
             TDiv AlexPosn      |
             TNEq AlexPosn      |
             TLess AlexPosn     |
             TGreater AlexPosn  |
             TLEq AlexPosn      |
             TGEq AlexPosn      |
             TAnd AlexPosn      |
             TOr AlexPosn       |
             TEq AlexPosn
             deriving (Eq,Show)


scan = alexScanTokens

lineNumber (AlexPn _ l _ ) = l
colNumber  (AlexPn _ _ c ) = c
             
}