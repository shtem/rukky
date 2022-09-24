import argparse
import sys

rukky_parser = argparse.ArgumentParser(
    description='Interpreter for the "rukky" functional programming language. Interprets code and outputs result.'
)

my_modes = rukky_parser.add_mutually_exclusive_group(required=True)
my_modes.add_argument('-s', '--shell', dest='shell', action='store_true', help='run interpreter in shell mode. Directly enter input to be interpreted')
my_modes.add_argument('-f', dest='file', type=str, help='run interpreter in file mode. Pass path to file to be interpreted')

my_output_types = rukky_parser.add_mutually_exclusive_group(required=False)
my_output_types.add_argument('-t', dest='token', action='store_true', help='print list of all tokens returned by lexer')
my_output_types.add_argument('-a', dest='ast', action='store_true', help='print AST returned by parser')
my_output_types.add_argument('-g', dest='table', action='store_true', help='print global symbol and function table returned by interpreter, with the result')


def shell():
    while True:
        try:
            text = input("rukky > ") + "\n "
            if text.strip() == "":
                continue
            
            return text

        except KeyboardInterrupt:
            sys.exit(0)