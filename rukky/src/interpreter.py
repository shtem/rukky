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
                message=f"line: {repr(self.parser.currTok.lineNo)} column: {repr(self.parser.currTok.columnNo)}. {message}",
            )
        )
        sys.exit(0)

    def reset(self):
        self.parser.reset()

    def interpret(self):
        programAST: ProgramASTNode = self.parser.parse()

        try:
            if programAST:
                return programAST.code_gen(), programAST.programContext.symbolTable
            else:
                return "", {}
        except Exception as e:
            self.error(str(e))

    # create wrapper that catches index error (out of range, index not a real), attribute error (symbol doesn't exist or not a list) in codegen for identifier asts in interpreter
    # create wrapper that catches type error (invalid type, has no type) in codegen for identifier ast in interpreter
    # create wrapper for value error (tried to find type of list, no lhs/rhs for operation or lhs/rhs == null, invalid operator)
    # create wrapper for ZeroDivisionError

    # fix line number and column number -> store in program context - in each ast node set line no and column no in context using token
    # when raising errors in context use context line and column number, in ast node use token line and column number (?) 