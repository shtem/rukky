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
    
    """
    elem -> "-" elem
        | "~" elem
        | "(" expr ")"
        | ID
        | ID ":" args
        | ID "[" expr "]"
        | REAL_LIT
        | BOOL_LIT
        | STRING_LIT
    """
    def elem(self):
        if self.currTok.type == TokenType.IDENT:
            tok = self.currTok
            ident = self.currTok.lexVal
            identAST = IdentifierASTNode(token=tok, type=None, ident=ident, listFlag=False)
            self.eat() # eat id
            if self.currTok == TokenType.COLON:
                self.eat() # eat :
                args = self.args()
                if self.currTok == TokenType.EOL:
                    self.eat() # eat \n
                    if args:
                        return CallExprASTNode(token=tok, callee=identAST, args=args) # id: args
                    else:
                        return identAST
                else:
                    self.error('newline')
            elif self.currTok == TokenType.LSQUARE:
                self.eat() # eat [
                index = self.expr()
                if self.currTok == TokenType.RSQUARE:
                    self.eat() # eat ]
                    if index:
                        identAST.set_index(index)
                        identAST.set_listFlag(True)
                        return identAST # id[expr]
                else:
                    self.error('"]"')
            else:
                return identAST # id
        elif self.currTok.type == TokenType.MINUS or self.currTok.type == TokenType.NOT:
            op = self.currTok
            self.eat() # eat - ~
            rhs = self.elem()
            if rhs:
                return UnaryExprASTNode(op=op, rhs=rhs)
        elif self.currTok.type == TokenType.LPAREN:
            self.eat() # eat (
            expr = self.expr()
            if self.currTok == TokenType.RPAREN:
                self.eat() # eat )
                if expr:
                    return expr
            else:
                self.error('")"')
        elif self.currTok.type == TokenType.REAL_LIT:
            realNum = RealASTNode(token=self.currTok, value=self.currTok.lexVal)
            self.eat() # eat real number
            return realNum
        elif self.currTok.type == TokenType.BOOL_LIT:
            boolVal = BoolASTNode(token=self.currTok, value=self.currTok.lexVal)
            self.eat() # eat boolean value
            return boolVal
        elif self.currTok.type == TokenType.STRING_LIT:
            strVal = StringASTNode(token=self.currTok, value=self.currTok.lexVal)
            self.eat() # eat string value
            return strVal

    """
    args -> arg_list
        | epsilon
    """ 
    def args(self):
        possibleStartToks = [TokenType.ID.value, TokenType.MINUS.value, TokenType.NOT.value, TokenType.LPAREN.value, 
                            TokenType.REAL_LIT.value, TokenType.BOOL_LIT.value, TokenType.STRING_LIT.value]
        
        if self.currTok in possibleStartToks: # id: args
            argList = self.arg_list()
            if argList:
                return argList
        elif self.currTok == TokenType.EOL: # id:
            return self.epsilon()
        else:
            self.error('list of expressins as arguments or a new line')

        return self.epsilon()

    
    """
    arg_list -> arg_list "," expr
        | expr
    """ 
    def args_list(self):
        argList = []

        expr = self.expr()
        if expr:
            argList.append(expr)
        
        while True:
            if self.currTok == TokenType.COMMA:
                self.eat() # eat ,
                expr = self.expr()
                if expr:
                    argList.append(expr)
            elif self.currTok == TokenType.EOL:
                break
            else:
                self.error('"," or a new line')
        
        return argList
    
    """
    epsilon -> 
    """ 
    def epsilon(self):
        return None
