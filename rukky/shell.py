import sys
from src.lexer import Lexer

while True:
    try:
        text = input("rukky > ") + "\n "
        if text.strip() == "":
            continue

        print(text)
        lex = Lexer(text)

        while lex.currChar:
            print(repr(lex.get_next_token()))
    except KeyboardInterrupt:
        sys.exit(0)
