import sys
from src.lexer import Lexer

while True:
    try:
        text = input('rukky > ')
        if text.strip() == "": continue

        print(text)
        lex = Lexer(text)

        while lex.currChar != None:
            print(repr(lex.get_next_token()))
    except KeyboardInterrupt:
        sys.exit(0)