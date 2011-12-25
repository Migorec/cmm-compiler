
cmm.exe : Lex.hs Parser.hs Main.hs CodeGen.hs
	ghc Main.hs -o cmm.exe -outputdir=out

Parser.hs : cexpr.y Lex.hs
	happy cexpr.y -o Parser.hs -i

Lex.hs : alexposn.x
	alex alexposn.x -o Lex.hs -i
	
	

	
