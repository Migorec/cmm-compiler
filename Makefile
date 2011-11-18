
cmm.exe : lex.hs parser.hs main.hs
	ghc main.hs -o cmm.exe -outputdir=out

parser.hs : cexpr.y lex.hs
	happy cexpr.y -o parser.hs

lex.hs : alexposn.x
	alex alexposn.x -o lex.hs
	
	

	
