from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter
import sys

while True:
    try:
        text = input("rukky > ") + "\n "
        if text.strip() == "":
            continue

        lex = Lexer(text=text)
        parser = Parser(lexer=lex)
        interp = Interpreter(parser=parser)

        print("\nInput\n------\n", text)

        print("\nTokens\n-------\n")
        while lex.currChar:
            print(repr(lex.get_next_token()))

        print("\nAST Tree\n---------\n")
        lex.reset()
        programAST = parser.parse()
        print(repr(programAST))

        print("\nResult\n---------\n")
        parser.reset()
        res, symb = interp.interpret()
        print(res, symb, "\n")

    except KeyboardInterrupt:
        sys.exit(0)
