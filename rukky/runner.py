from common.cmd_tools import rukky_parser, shell
from src.interpreter import Interpreter
from src.parser import Parser
from src.lexer import Lexer
import sys, os


def main():

    args = rukky_parser.parse_args()
    fileName = None
    text = ''

    if args.file:
        fileName = args.file

        if not os.path.exists(fileName):
            print('The file path specified does not exist')
            sys.exit()

        with open(fileName, "r") as file:
            text = file.read()
    
    if args.shell:
        text = shell()

    lex = Lexer(text=text)
    parser = Parser(lexer=lex)
    interp = Interpreter(parser=parser)

    if args.token:
        lex.reset()
        print("\nTokens\n-------")
        while lex.currChar:
            print(repr(lex.get_next_token()))
        sys.exit(0)
    if args.ast:
        lex.reset()
        programAST = parser.parse()
        print("\nAST Tree\n---------")
        print(repr(programAST))
        sys.exit(0)

    parser.reset()
    _, context = interp.interpret()

    if args.table:
        print("\nGlobal Tables\n-------------")
        print(f"Symbol Table: {context.symbolTable}\nFunction Table: {context.funcTable}")

main()
