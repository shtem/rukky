# Runner converts program to stream and passes it to lexer, then lexer tokens to parser, then AST to interpreter, then interpreter evaluates it and reuslt is returned
from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter
import sys


def main():
    fileName = sys.argv[1]

    with open(fileName, "r") as file:
        text = file.read()

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


main()
