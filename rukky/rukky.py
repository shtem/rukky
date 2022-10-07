from src.interpreter import Interpreter
from src.parser import Parser
from src.lexer import Lexer
import os, sys, time, argparse

rukkyArgParser = argparse.ArgumentParser(
    description='Interpreter for the "rukky" programming language. Interprets code and outputs result.'
)

myModes = rukkyArgParser.add_mutually_exclusive_group(required=True)
myModes.add_argument(
    "-s",
    "--shell",
    dest="shell",
    action="store_true",
    help="run interpreter in shell mode. Directly enter input into the REPL to be interpreted",
)
myModes.add_argument(
    "-f",
    dest="file",
    type=str,
    help="run interpreter in file mode. Pass path to file to be interpreted",
)

myOutputTypes = rukkyArgParser.add_mutually_exclusive_group(required=False)
myOutputTypes.add_argument(
    "-t",
    "--tokens",
    dest="tokens",
    action="store_true",
    help="outputs list of all tokens returned by lexer",
)
myOutputTypes.add_argument(
    "-a",
    "--ast",
    dest="ast",
    action="store_true",
    help="outputs AST returned by parser",
)
myOutputTypes.add_argument(
    "-g",
    "--global",
    dest="table",
    action="store_true",
    help="outputs global symbol tables returned by interpreter, along with the result",
)
myOutputTypes.add_argument(
    "-d",
    "--duration",
    dest="duration",
    action="store_true",
    help="outputs time it takes to interpret inputted program, along with the result",
)


def execute(args, text):
    lex = Lexer(text=text)
    parser = Parser(lexer=lex)
    interp = Interpreter(parser=parser)

    if args.tokens:
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

    if args.duration:
        startTime = time.time()

    interp.reset()
    result, programAST = interp.interpret()

    if result != None and programAST and args.shell:
        isDict = isinstance(result, dict)
        print(
            programAST.globalContext._display_builtin_helper(
                strOut=str(result), isDict=isDict
            )
        )

    if args.duration:
        duration = time.time() - startTime
        print("\nDuration\n---------")
        print(f"{duration} seconds\n")
        return

    if args.table and programAST:
        print("\nGlobal Tables\n-------------")
        print(
            f"Symbol Table: {programAST.globalContext.symbolTable}\nFunction Table: {programAST.globalContext.funcTable}\nClass Table: {programAST.globalContext.classTable}\n"
        )
        return


def runner():
    args = rukkyArgParser.parse_args()
    filePath = ""
    text = ""

    if args.file:
        filePath = args.file

        if not os.path.exists(filePath):
            print(f"{filePath}: The file path does not exist")
            sys.exit(0)

        with open(filePath, "r") as file:
            text = file.read()

        if text.strip() == "":
            return

        execute(args=args, text=text)
        return

    if args.shell:
        version = "rukky v1.0.0 REPL"
        helpOne = '\nType "begin" for multiline input, "end" to evaluate multiline input, "bye" to exit REPL \nType "tokens" for token output, "ast" for AST output,'
        helpTwo = '"global" for global symbol table output, "duration" for duration time output\nType output option name again to disable the output and "help" to display this message again.'
        shellHelp = version + helpOne + helpTwo

        print(shellHelp)
        try:
            while True:
                text = input("rukky> ")

                if text.strip() == "help":
                    print(shellHelp)
                    continue

                if text.strip() == "tokens":
                    args.tokens = not args.tokens
                    args.ast = False
                    args.table = False
                    args.duration = False
                    continue

                if text.strip() == "ast":
                    args.ast = not args.ast
                    args.tokens = False
                    args.table = False
                    args.duration = False
                    continue

                if text.strip() == "global":
                    args.table = not args.table
                    args.ast = False
                    args.tokens = False
                    args.duration = False
                    continue

                if text.strip() == "duration":
                    args.duration = not args.duration
                    args.ast = False
                    args.tokens = False
                    args.table = False
                    continue

                if text.strip() == "bye":
                    sys.exit(0)

                if text.strip() == "begin":
                    buffer = []

                    while True:
                        line = input(". ")

                        if line.strip() == "end":
                            break
                        buffer.append(line.strip())

                    print(">>")
                    text = "\n".join(buffer)

                if text.strip() == "":
                    continue

                try:
                    execute(args=args, text=text)
                except SystemExit:
                    pass
        except (KeyboardInterrupt, EOFError):
            sys.exit(0)


if __name__ == "__main__":
    runner()
