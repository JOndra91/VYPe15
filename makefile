all: 
	happy Parser.y -o src/VYPe15/Internal/Parser.hs
	alex Lexer.x -o src/VYPe15/Internal/Lexer.hs
	stack build
