all : parser lexer
	stack build

parser : Parser.y
	happy Parser.y -o src/VYPe15/Internal/Parser.hs

lexer : Lexer.x
	alex Lexer.x -o src/VYPe15/Internal/Lexer.hs

