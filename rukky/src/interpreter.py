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

    def reset(self):
        self.parser.reset()

    def interpret(self):
        programAST: ProgramASTNode = self.parser.parse()

        if programAST:
            return programAST.code_gen(), programAST.programContext.symbolTable
        else:
            return "", {}

    # create wrapper that catches index error (out of range, index not a real), attribute error (symbol doesn't exist or not a list) in codegen for identifier asts in interpreter
    # create wrapper that catches type error (invalid type, has no type) in codegen for identifier ast in interpreter
    # create wrapper for value error (tried to find type of list, no lhs/rhs for operation or lhs/rhs == null, invalid operator)
    # create wrapper for ZeroDivisionError
    # - create specific errors that are children of python errors so I can try catch here (?)
    # - better idea !! - just pass message to exception itself and capture message and pass to parser error !!

    # store list ast type as list type in symbol and func entries but when assigning or appending is taking place check ast type matches first
    # always convert index to int in identifier ast and assignment ast in codegen

    # reserved keyword - all take 1 argument apart from rand which takes 0 and type which takes 2, and pi, eul and null which are constants
    #   display, getStr - any, len - list|str, type - real|bool|str|null, getReal - real|bool|str
    #   rand, floor, ceil, sqrt, log, sin, cos, tan - real

    # create new context everytime stmts block is created, create global context in program ast

    # append in binary operation, doesn't catch the case when list is empty, could real[] << true, could create Real/Str/BoolList object
