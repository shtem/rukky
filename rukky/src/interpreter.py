from common.errors import SemanticError
from src.parser import Parser
from data.ast import *
import sys


class Interpreter:
    def __init__(self, parser: Parser):
        self.parser = parser

    def error(self, message: str):
        print(
            SemanticError(
                message=f'Semantic Error on line: {repr(self.parser.currTok.lineNo)} column: line: {repr(self.parser.currTok.columnNo)}". {message}',
            )
        )
        sys.exit(0)

    # create wrapper that catches index error, type error (symbol doesn't exist or not a list) in interpreter
    # store list ast type as list type in symbol and func entries but when assigning or appending is taking place check ast type matches first
