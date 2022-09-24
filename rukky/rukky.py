from src.interpreter import Interpreter
from src.parser import Parser
from src.lexer import Lexer
import sys, os, argparse

rukky_arg_parser = argparse.ArgumentParser(
    description='Interpreter for the "rukky" functional programming language. Interprets code and outputs result.'
)

my_modes = rukky_arg_parser.add_mutually_exclusive_group(required=True)
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

my_output_types = rukky_arg_parser.add_mutually_exclusive_group(required=False)
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


def execute(args, text):
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


def runner():
    args = rukky_arg_parser.parse_args()
    fileName = ""
    text = ""

    if args.file:
        fileName = args.file

        if not os.path.exists(fileName):
            print(f"{fileName}: The file path does not exist")
            sys.exit()

        with open(fileName, "r") as file:
            text = file.read()

        execute(args=args, text=text)
        return

    if args.shell:
        shellHelp = 'rukky 1.0.0 REPL\nType "begin" for multiline input, "end" to evaluate multiline input, "bye" to exit REPL and "help" to display this message again.'
        print(shellHelp)
        try:
            while True:
                text = input("rukky> ")
                if text.strip() == "":
                    continue

                if text.strip() == "bye":
                    sys.exit(0)

                if text.strip() == "help":
                    print(shellHelp)
                    continue

                if text.strip() == "begin":
                    buffer = []

                    while True:
                        line = input(". ")
                        if "end" == line.strip():
                            break
                        buffer.append(line.strip())

                    print(">>")
                    text = "\n".join(buffer)

                text += "\n "

                try:
                    execute(args=args, text=text)
                except SystemExit:
                    pass
        except KeyboardInterrupt:
            sys.exit(0)


if __name__ == "__main__":
    runner()
