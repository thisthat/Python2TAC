CC= ghc
GHCI= ghci
FLAGHAPPY= -gcai
EXECUTABLE=es9
EXAMPLE=example.py
all: compile

compile: Lexer.hs Parser.hs
	$(CC) Main.hs -o $(EXECUTABLE)

Parser.hs: Parser.y
	happy $(FLAGHAPPY) Parser.y

Lexer.hs: Lexer.x
	alex Lexer.x

.PHONY: clean
clean:
	-rm *.hi
	-rm *.o

demo: compile
	./$(EXECUTABLE) < $(EXAMPLE)

# DO NOT DELETE: Beginning of Haskell dependencies
Token.o : Token.hs
Abs.o : Abs.hs
Env.o : Env.hs
Env.o : Abs.hi
TypeChecker.o : TypeChecker.hs
TypeChecker.o : Env.hi
TypeChecker.o : Abs.hi
Tac.o : Tac.hs
Tac.o : Env.hi
Tac.o : TypeChecker.hi
Tac.o : Abs.hi
Lexer.o : Lexer.hs
Lexer.o : Token.hi
Parser.o : Parser.hs
Parser.o : Abs.hi
Parser.o : Token.hi
Parser.o : Lexer.hi
Main.o : Main.hs
Main.o : Token.hi
Main.o : Tac.hi
Main.o : Abs.hi
Main.o : TypeChecker.hi
Main.o : Parser.hi
Main.o : Lexer.hi
# DO NOT DELETE: End of Haskell dependencies
