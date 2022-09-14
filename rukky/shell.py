import sys
from src.lexer import Lexer
from src.parser import Parser

while True:
    try:
        text = input("rukky > ") + "\n "
        if text.strip() == "":
            continue

        print(text)
        lex = Lexer(text=text)
        parser = Parser(lexer=lex)

        while lex.currChar:
            print(repr(lex.get_next_token()))

        lex.reset()
        programAST = parser.parse()
        print(repr(programAST))
    except KeyboardInterrupt:
        sys.exit(0)
