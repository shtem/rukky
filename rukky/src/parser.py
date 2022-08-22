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
    
    def peek(self):
        return self.lexer.peek_next_token()

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

    """
    expr -> ID ":=" expr
        | ID ":=" "[" args "]"
        | ID "[" expr "]" ":=" expr
        | ID "<<" expr
        | disjunc
    """
    def expr(self):
        if self.currTok.type == TokenType.ID:
            tok = self.currTok
            if self.peek().type == TokenType.ASSIGN:
                ident = tok.lexVal
                self.eat() # eat id
                identAST = IdentifierASTNode(token=tok, type=None, ident=ident, listFlag=False)
                self.eat() # eat :=

            

    """
    disjunc -> disjunc "||" conjunc
        | conjunc
    """
    def disjunc(self):
        possibleStartToks = [TokenType.EOL, TokenType.RPAREN, TokenType.COMMA]

        lhs = self.conjunc()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type == TokenType.OR:
                op = self.currTok
                self.eat() # eat ||
                rhs = self.conjunc()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleStartToks:
                return lhs
            else:
                self.error('binary operator or ")" or "," or newline')
    
    """
    conjunc -> conjunc "&&" equiv
        | equiv
    """
    def conjunc(self):
        possibleStartToks = [TokenType.OR, TokenType.EOL, TokenType.RPAREN, TokenType.COMMA]

        lhs = self.equiv()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type == TokenType.AND:
                op = self.currTok
                self.eat() # eat &&
                rhs = self.equiv()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleStartToks:
                return lhs
            else:
                self.error('binary operator or ")" or "," or newline')
    
    """
    equiv -> equiv "<>" ineq
        | equiv "<!" ineq
        | ineq
    """
    def equiv(self):
        possibleStartToks = [TokenType.AND, TokenType.OR,
                            TokenType.EOL, TokenType.RPAREN, TokenType.COMMA]

        lhs = self.ineq()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type == TokenType.NE or self.currTok.type == TokenType.EQ:
                op = self.currTok
                self.eat() # eat <> <!
                rhs = self.ineq()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleStartToks:
                return lhs
            else:
                self.error('binary operator or ")" or "," or newline')
    
    """
    ineq -> ineq "<=" term
        | ineq "<" term
        | ineq ">=" term
        | ineq ">" term
        | term
    """
    def ineq(self):
        possibleStartToks = [TokenType.EQ, TokenType.NE, TokenType.AND, TokenType.OR,
                            TokenType.EOL, TokenType.RPAREN, TokenType.COMMA]

        lhs = self.term()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type in [TokenType.GT, TokenType.GE, TokenType.LT, TokenType.LE]:
                op = self.currTok
                self.eat() # eat <= < >= >
                rhs = self.term()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleStartToks:
                return lhs
            else:
                self.error('binary operator or ")" or "," or newline')
    
    """
    term -> term "+" factor
        | term "-" factor
        | factor
    """
    def term(self):
        possibleStartToks = [TokenType.GT, TokenType.GE, TokenType.LT,
                            TokenType.LE, TokenType.EQ, TokenType.NE, TokenType.AND, TokenType.OR,
                            TokenType.EOL, TokenType.RPAREN, TokenType.COMMA]

        lhs = self.factor()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type == TokenType.MINUS or self.currTok.type == TokenType.PLUS:
                op = self.currTok
                self.eat() # eat + -
                rhs = self.factor()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleStartToks:
                return lhs
            else:
                self.error('binary operator or ")" or "," or newline')

    """
    factor -> factor "*" expo
        | factor "/" expo
        | factor "//" expo
        | factor "%" expo
        | expo
    """
    def factor(self):
        possibleStartToks = [TokenType.MINUS, TokenType.PLUS, TokenType.GT, TokenType.GE, TokenType.LT,
                            TokenType.LE, TokenType.EQ, TokenType.NE, TokenType.AND, TokenType.OR,
                            TokenType.EOL, TokenType.RPAREN, TokenType.COMMA]

        lhs = self.expo()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type in [TokenType.MOD, TokenType.INT_DIV, TokenType.FLOAT_DIV, TokenType.MUL]:
                op = self.currTok
                self.eat() # eat * / // %
                rhs = self.expo()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleStartToks:
                return lhs
            else:
                self.error('binary operator or ")" or "," or newline')

    """
    expo -> expo "^" elem 
        | elem
    """
    def expo(self):
        possibleStartToks = [TokenType.MOD, TokenType.INT_DIV, TokenType.FLOAT_DIV, TokenType.MUL, 
                            TokenType.MINUS, TokenType.PLUS, TokenType.GT, TokenType.GE, TokenType.LT,
                            TokenType.LE, TokenType.EQ, TokenType.NE, TokenType.AND, TokenType.OR,
                            TokenType.EOL, TokenType.RPAREN, TokenType.COMMA]

        lhs = self.elem()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type == TokenType.EXP:
                op = self.currTok
                self.eat() # eat ^
                rhs = self.elem()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleStartToks:
                return lhs
            else:
                self.error('binary operator or ")" or "," or newline')

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
            if self.currTok.type == TokenType.COLON:
                self.eat() # eat :
                args = self.args()
                if self.currTok.type == TokenType.EOL:
                    self.eat() # eat \n
                    if args:
                        return CallExprASTNode(token=tok, callee=identAST, args=args) # id: args
                    else:
                        return identAST
                else:
                    self.error('newline')
            elif self.currTok.type == TokenType.LSQUARE:
                self.eat() # eat [
                index = self.expr()
                if self.currTok.type == TokenType.RSQUARE:
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
            if self.currTok.type == TokenType.RPAREN:
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
        possibleStartToks = [TokenType.ID, TokenType.MINUS, TokenType.NOT, TokenType.LPAREN, 
                            TokenType.REAL_LIT, TokenType.BOOL_LIT, TokenType.STRING_LIT]
        
        if self.currTok.type in possibleStartToks: # id: args
            argList = self.arg_list()
            if argList:
                return argList
        elif self.currTok.type == TokenType.EOL: # id:
            return self.epsilon()
        else:
            self.error('list of expressins as arguments or newline')

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
            if self.currTok.type == TokenType.COMMA:
                self.eat() # eat ,
                expr = self.expr()
                if expr:
                    argList.append(expr)
            elif self.currTok.type == TokenType.EOL:
                return argList
            else:
                self.error('"," or newline')
    
    """
    epsilon -> 
    """ 
    def epsilon(self):
        return None
