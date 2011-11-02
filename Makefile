
lex.hs : alexposn.x
	alex alexposn.x -o lex.hs
	
parser.hs : cexpr.y lex.hs
	happy cexpr.y -o parser.hs
	
run : 
	ghci parser.hs