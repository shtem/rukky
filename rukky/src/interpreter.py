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
                message=message,
            )
        )
        sys.exit(0)

    def reset(self):
        self.parser.reset()

    def interpret(self):
        programAST: ProgramASTNode = self.parser.parse()

        try:
            if programAST:
                return programAST.code_gen(), programAST
            else:
                return "", {}
        except Exception as e:
            self.error(str(e))
