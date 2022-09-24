from src.interpreter import Interpreter
from src.parser import Parser
from src.lexer import Lexer
import sys, os, argparse

rukky_parser = argparse.ArgumentParser(
    description='Interpreter for the "rukky" functional programming language. Interprets code and outputs result.'
)

my_modes = rukky_parser.add_mutually_exclusive_group(required=True)
my_modes.add_argument(
    "-s",
    "--shell",
    dest="shell",
    action="store_true",
    help="run interpreter in shell mode. Directly enter input to be interpreted",
)
my_modes.add_argument(
    "-f",
    dest="file",
    type=str,
    help="run interpreter in file mode. Pass path to file to be interpreted",
)

my_output_types = rukky_parser.add_mutually_exclusive_group(required=False)
my_output_types.add_argument(
    "-t",
    dest="token",
    action="store_true",
    help="print list of all tokens returned by lexer",
)
my_output_types.add_argument(
    "-a", dest="ast", action="store_true", help="print AST returned by parser"
)
my_output_types.add_argument(
    "-g",
    dest="table",
    action="store_true",
    help="print global symbol and function table returned by interpreter, with the result",
)


def execute_interp(args, text):
    lex = Lexer(text=text)
    parser = Parser(lexer=lex)
    interp = Interpreter(parser=parser)

    if args.token:
        lex.reset()
        print("\nTokens\n-------")
        while lex.currChar:
            print(repr(lex.get_next_token()))
        print("\n")
        return

    if args.ast:
        parser.reset()
        programAST = parser.parse()
        print("\nAST Tree\n---------")
        print(repr(programAST), "\n")
        return

    interp.reset()
    result, programAST = interp.interpret()

    if result != None and programAST and args.shell:
        print(programAST.programContext._display_builtin_helper(strOut=str(result)))

    if args.table:
        print("\nGlobal Tables\n-------------")
        print(
            f"Symbol Table: {programAST.programContext.symbolTable}\nFunction Table: {programAST.programContext.funcTable}\n"
        )
        return


def main():
    args = rukky_parser.parse_args()
    fileName = ""
    text = ""

    if args.file:
        fileName = args.file

        if not os.path.exists(fileName):
            print("The file path specified does not exist")
            sys.exit()

        with open(fileName, "r") as file:
            text = file.read()

        execute_interp(args=args, text=text)

    if args.shell:
        print(
            'rukky 1.0.0 REPL\nType "start" for multiline input, "end" to evaluate multiline input, "bye" to exit.'
        )
        try:
            while True:
                text = input("rukky> ")
                if text.strip() == "":
                    continue

                if text.strip() == "bye":
                    sys.exit(0)

                if text.strip() == "start":
                    buffer = []

                    while True:
                        line = input(". ")
                        if "end" == line.strip():
                            break
                        buffer.append(line.strip())

                    text = "\n".join(buffer)

                text += "\n "
                execute_interp(args=args, text=text)
        except KeyboardInterrupt:
            sys.exit(0)


main()
