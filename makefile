all : parser lexer
	stack build

parser : Parser.y
	mkdir -p src/VYPe15/Internal/
	happy Parser.y -o src/VYPe15/Internal/Parser.hs

lexer : Lexer.x
	mkdir -p src/VYPe15/Internal/
	alex Lexer.x -o src/VYPe15/Internal/Lexer.hs

