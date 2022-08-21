# Parser builds AST from tokens, if its a valid program builds AST, If invalid can't build tree report error
# Recursive Descent Parser
# error handler in class - syntax error

from common.lex_enums import TokenType
from common.errors import ParserError
from data.ast import *
import sys

class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.currTok = None 

    def error(self, message):
        print(
            ParserError(
                token=self.currTok,
                message=f'Unexpected Token "{self.currTok}" on line: {self.currTok.lineNo} column: {self.currTok.columnNo}. Expected {message}'
            )
        )
        sys.exit(0)
    
    def eat(self):
        self.currTok = self.lexer.get_next_token() # eat current token by assigning current token to next token in the input
    
    def parse(self):
        self.eat() # set current token to first token in the input

        return self.program()

    def program(self):
        pass
    
    def decl_list(self):
        pass

    def decl(self):
        pass

    def var_type(self):
        pass

    def func_type(self):
        pass

    def func_decl(self):
        pass

    def params(self):
        pass

    def param_list(self):
        pass

    def param(self):
        pass

    def block(self):
        pass

    def stmt_list(self):
        pass

    def stmt(self):
        pass

    def decl_stmt(self):
        pass

    def expr_stmt(self):
        pass

    def for_stmt(self):
        pass

    def while_stmt(self):
        pass

    def if_stmt(self):
        pass

    def elif_stmt_list(self):
        pass

    def elif_stmt(self):
        pass

    def else_stmt(self):
        pass

    def return_stmt(self):
        pass

    def break_stmt(self):
        pass

    def expr(self):
        pass

    def disjunc(self):
        pass

    def conjunc(self):
        pass

    def equiv(self):
        pass

    def ineq(self):
        pass

    def term(self):
        pass

    def factor(self):
        pass

    def expo(self):
        pass

    def elem(self):
        pass

    def id_body(self):
        pass

    def args(self):
        pass

    def args_list(self):
        pass

    def epsilon(self):
        return None
