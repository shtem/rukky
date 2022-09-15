import sys
from src.lexer import Lexer
from src.parser import Parser

while True:
    try:
        text = input("rukky > ") + "\n "
        if text.strip() == "":
            continue

        lex = Lexer(text=text)
        parser = Parser(lexer=lex)

        print("\nInput\n------\n", text)

        print("\nTokens\n-------\n")
        while lex.currChar:
            print(repr(lex.get_next_token()))

        print("\nAST Tree\n---------\n")
        lex.reset()
        programAST = parser.parse()
        print(repr(programAST))

    except KeyboardInterrupt:
        sys.exit(0)
