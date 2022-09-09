from common.lex_enums import TokenType
from common.errors import ParserError
from src.lexer import Lexer
from data.ast import *
import math
import sys


class Parser:
    def __init__(self, lexer: Lexer):
        self.lexer = lexer
        self.currTok: Token = None

    def error(self, missing: str):
        print(
            ParserError(
                message=f'Unexpected Token "{repr(self.currTok)}". Expected {missing}',
            )
        )
        sys.exit(0)

    def eat(self):
        self.currTok = (
            self.lexer.get_next_token()
        )  # eat current token by assigning current token to next token in the input

    def peek(self):
        return self.lexer.peek_next_token()

    def parse(self):
        self.eat()  # set current token to first token in the input

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

    """
    expr_stmt -> expr EOL
            | EOL
    """

    def expr_stmt(self):
        if self.currTok.type == TokenType.EOL:
            self.eat()  # eat \n
            return self.epsilon()
        else:
            expr = self.expr()
            if self.currTok.type == TokenType.EOL:
                self.eat()  # eat \n
                if expr:
                    return expr
            else:
                self.error("newline")

    """
    expr_stmt -> expr EOL
            | EOL
    """

    def for_stmt(self):
        if self.currTok.type == TokenType.FOR:
            tok = self.currTok
            self.eat()  # eat 'for'
            if self.currTok.type == TokenType.RES_COLON:
                self.eat()  # eat ::
                if self.currTok.type == TokenType.ID:
                    identAST = IdentifierASTNode(
                        token=self.currTok,
                        type="real",
                        ident=self.currTok.lexVal,
                        index=None,
                        listFlag=False,
                    )
                    self.eat()  # eat id
                    if self.currTok.type == TokenType.ASSIGN:
                        self.eat()  # eat :=
                        start = self.expr()
                        if self.currTok.type == TokenType.COLON:
                            self.eat()  # eat :
                            end = self.expr()
                            if self.currTok.type == TokenType.COLON:
                                self.eat()  # eat :
                                increment = self.expr()
                                if start and end and increment:
                                    body = self.block()
                                    if body:
                                        return ForStmtASTNode(
                                            token=tok,
                                            counter=identAST,
                                            start=start,
                                            end=end,
                                            increment=increment,
                                            forBody=body,
                                        )
                                else:
                                    self.error(
                                        "expression as start or end or increment value of for loop"
                                    )
                            else:
                                self.error('":"')
                        else:
                            self.error('":"')
                    else:
                        self.error('":="')
                else:
                    self.error("identifier")
            else:
                self.error('"::"')
        else:
            return self.epsilon()

    """
    while_stmt -> "while" "::" expr block
    """

    def while_stmt(self):
        if self.currTok.type == TokenType.WHILE:
            tok = self.currTok
            self.eat()  # eat 'while'
            if self.currTok.type == TokenType.RES_COLON:
                self.eat()  # eat ::
                cond = self.expr()
                if cond:
                    body = self.block()
                    if body:
                        return WhileStmtASTNode(token=tok, cond=cond, whileBody=body)
                else:
                    self.error("expression as while condition")
            else:
                self.error('"::"')
        else:
            return self.epsilon()

    """
    if_stmt -> "if" "::" expr block elif_stmt_list else_stmt
    """

    def if_stmt(self):
        if self.currTok.type == TokenType.IF:
            tok = self.currTok
            self.eat()  # eat 'if'
            if self.currTok.type == TokenType.RES_COLON:
                self.eat()  # eat ::
                cond = self.expr()
                if cond:
                    body = self.block()
                    if body:
                        elifStmtList = self.elif_stmt_list()
                        elseBody = self.else_stmt()
                        return IfStmtASTNode(
                            token=tok,
                            cond=cond,
                            ifBody=body,
                            elifStmts=elifStmtList,
                            elseBody=elseBody,
                        )

                else:
                    self.error("expression as if condition")
            else:
                self.error('"::"')
        else:
            return self.epsilon()

    """
    elif_stmt_list -> elif_stmt_list elif_stmt
                | elif_stmt
    """

    def elif_stmt_list(self):
        possibleEndToks = [
            TokenType.ID,
            TokenType.MINUS,
            TokenType.NOT,
            TokenType.LPAREN,
            TokenType.REAL_LIT,
            TokenType.BOOL_LIT,
            TokenType.STRING_LIT,
            TokenType.EOL,
            TokenType.RBRACE,
            TokenType.IF,
            TokenType.ELSE,
            TokenType.FOR,
            TokenType.WHILE,
            TokenType.RETURN,
            TokenType.BREAK,
            TokenType.VOID,
            TokenType.REAL,
            TokenType.BOOL,
            TokenType.STRING,
            TokenType.DISPLAY,
            TokenType.LENGTH,
            TokenType.NULL,
            TokenType.PI,
            TokenType.EULER,
        ]

        elifStmtList = []

        elifStmt = self.elif_stmt()
        if elifStmt:
            elifStmtList.append(elifStmt)

        while True:
            if self.currTok.type == TokenType.ELIF:
                elifStmt = self.elif_stmt()
                if elifStmt:
                    elifStmtList.append(elifStmt)
            elif self.currTok.type in possibleEndToks:
                return elifStmtList
            else:
                self.error('newline or "else"')

    """
    elif_stmt -> "elif" "::" expr block
                | epsilon
    """

    def elif_stmt(self):
        if self.currTok.type == TokenType.ELIF:
            tok = self.currTok
            self.eat()  # eat 'elif'
            if self.currTok.type == TokenType.RES_COLON:
                self.eat()  # eat ::
                cond = self.expr()
                if cond:
                    body = self.block()
                    if body:
                        return ElifStmtASTNode(token=tok, cond=cond, elifBody=body)
                else:
                    self.error("expression as elif condition")
            else:
                self.error('"::"')
        else:
            return self.epsilon()

    """
    else_stmt -> "else" "::" block
                | epsilon
    """

    def else_stmt(self):
        if self.currTok.type == TokenType.ELSE:
            self.eat()  # eat 'else'
            if self.currTok.type == TokenType.RES_COLON:
                self.eat()  # eat ::
                body = self.block()
                if body:
                    return body
            else:
                self.error('"::"')
        else:
            return self.epsilon()

    """
    return_stmt -> "return" ":" EOL
                | "return" ":" expr EOL
    """

    def return_stmt(self):
        if self.currTok.type == TokenType.RETURN:
            tok = self.currTok
            self.eat()  # eat 'return'
            if self.currTok.type == TokenType.COLON:
                self.eat()  # eat :
                if self.currTok.type == TokenType.EOL:
                    self.eat()  # eat \n
                    return ReturnStmtASTNode(token=tok, returnBody=None)
                else:
                    body = self.expr()
                    if self.currTok.type == TokenType.EOL:
                        self.eat()  # eat \n
                        if body:
                            return ReturnStmtASTNode(token=tok, returnBody=body)
                    else:
                        self.error("newline")
            else:
                self.error('":"')
        else:
            return self.epsilon()

    """
    break_stmt -> "break" ":" EOL
    """

    def break_stmt(self):
        if self.currTok.type == TokenType.BREAK:
            tok = self.currTok
            self.eat()  # eat 'break'
            if self.currTok.type == TokenType.COLON:
                self.eat()  # eat :
                if self.currTok.type == TokenType.EOL:
                    self.eat()  # eat \n
                    return BreakStmtASTNode(token=tok)
                else:
                    self.error("newline")
            else:
                self.error('":"')
        else:
            return self.epsilon()

    """
    expr -> ID ":=" expr
        | ID ":=" "[" args "]"
        | ID "@" expr ":=" expr
        | ID "<<" expr
        | disjunc
    """

    def expr(self):
        if self.currTok.type == TokenType.ID:
            tok = self.currTok
            if self.peek().type == TokenType.ASSIGN:
                ident = tok.lexVal
                self.eat()  # eat id
                identAST = IdentifierASTNode(
                    token=tok, type=None, ident=ident, index=None, listFlag=False
                )
                self.eat()  # eat :=
                if self.currTok == TokenType.LSQUARE:
                    listTok = self.currTok
                    self.eat()  # eat [
                    elems = self.args()
                    if self.currTok.type == TokenType.RSQUARE:
                        self.eat()  # eat ]
                        if elems:
                            identAST.set_listFlag(True)
                            listAST = ListASTNode(token=listTok, elems=elems)
                            return AssignASTNode(token=tok, var=identAST, value=listAST)
                        else:
                            identAST.set_listFlag(True)
                            listAST = ListASTNode(token=listTok, elems=[])
                            return AssignASTNode(token=tok, var=identAST, value=listAST)
                    else:
                        self.error('"]"')
                else:
                    val = self.expr()
                    if val:
                        return AssignASTNode(token=tok, var=identAST, value=val)
            elif self.peek().type == TokenType.APPEND:
                ident = tok.lexVal
                self.eat()  # eat id
                identAST = IdentifierASTNode(
                    token=tok, type=None, ident=ident, index=None, listFlag=True
                )
                self.eat()  # eat <<
                val = self.expr()
                if val:
                    return AssignASTNode(token=tok, var=identAST, value=val)
            elif self.peek().type == TokenType.LIST_ASSIGN:
                ident = tok.lexVal
                self.eat()  # eat id
                identAST = IdentifierASTNode(
                    token=tok, type=None, ident=ident, index=None, listFlag=True
                )
                self.eat()  # eat @
                index = self.expr()
                if index:
                    identAST.set_index(index)
                    if self.currTok.type == TokenType.ASSIGN:
                        self.eat()  # eat :=
                        val = self.expr()
                        if val:
                            return AssignASTNode(token=tok, var=identAST, value=val)
                    else:
                        self.error('":="')
                else:
                    self.error("valid expression as index")
            else:
                return self.disjunc()
        else:
            return self.disjunc()

    """
    disjunc -> disjunc "||" conjunc
        | conjunc
    """

    def disjunc(self):
        possibleEndToks = [
            TokenType.EOL,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.ASSIGN,
            TokenType.COMMA,
        ]

        lhs = self.conjunc()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type == TokenType.OR:
                op = self.currTok
                self.eat()  # eat ||
                rhs = self.conjunc()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleEndToks:
                return lhs
            else:
                self.error('binary operator or ")" or "]" or ":=" or "," or newline')

    """
    conjunc -> conjunc "&&" equiv
        | equiv
    """

    def conjunc(self):
        possibleEndToks = [
            TokenType.OR,
            TokenType.EOL,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.ASSIGN,
            TokenType.COMMA,
        ]

        lhs = self.equiv()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type == TokenType.AND:
                op = self.currTok
                self.eat()  # eat &&
                rhs = self.equiv()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleEndToks:
                return lhs
            else:
                self.error('binary operator or ")" or "]" or ":=" or "," or newline')

    """
    equiv -> equiv "<>" ineq
        | equiv "<!" ineq
        | ineq
    """

    def equiv(self):
        possibleEndToks = [
            TokenType.AND,
            TokenType.OR,
            TokenType.EOL,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.ASSIGN,
            TokenType.COMMA,
        ]

        lhs = self.ineq()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type == TokenType.NE or self.currTok.type == TokenType.EQ:
                op = self.currTok
                self.eat()  # eat <> <!
                rhs = self.ineq()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleEndToks:
                return lhs
            else:
                self.error('binary operator or ")" or "]" or ":=" or "," or newline')

    """
    ineq -> ineq "<=" term
        | ineq "<" term
        | ineq ">=" term
        | ineq ">" term
        | term
    """

    def ineq(self):
        possibleEndToks = [
            TokenType.EQ,
            TokenType.NE,
            TokenType.AND,
            TokenType.OR,
            TokenType.EOL,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.ASSIGN,
            TokenType.COMMA,
        ]

        lhs = self.term()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type in [
                TokenType.GT,
                TokenType.GE,
                TokenType.LT,
                TokenType.LE,
            ]:
                op = self.currTok
                self.eat()  # eat <= < >= >
                rhs = self.term()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleEndToks:
                return lhs
            else:
                self.error('binary operator or ")" or "]" or ":=" or "," or newline')

    """
    term -> term "+" factor
        | term "-" factor
        | factor
    """

    def term(self):
        possibleEndToks = [
            TokenType.GT,
            TokenType.GE,
            TokenType.LT,
            TokenType.LE,
            TokenType.EQ,
            TokenType.NE,
            TokenType.AND,
            TokenType.OR,
            TokenType.EOL,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.ASSIGN,
            TokenType.COMMA,
        ]

        lhs = self.factor()

        if not lhs:
            return self.epsilon()

        while True:
            if (
                self.currTok.type == TokenType.MINUS
                or self.currTok.type == TokenType.PLUS
            ):
                op = self.currTok
                self.eat()  # eat + -
                rhs = self.factor()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleEndToks:
                return lhs
            else:
                self.error('binary operator or ")" or "]" or ":=" or "," or newline')

    """
    factor -> factor "*" expo
        | factor "/" expo
        | factor "//" expo
        | factor "%" expo
        | expo
    """

    def factor(self):
        possibleEndToks = [
            TokenType.MINUS,
            TokenType.PLUS,
            TokenType.GT,
            TokenType.GE,
            TokenType.LT,
            TokenType.LE,
            TokenType.EQ,
            TokenType.NE,
            TokenType.AND,
            TokenType.OR,
            TokenType.EOL,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.ASSIGN,
            TokenType.COMMA,
        ]

        lhs = self.expo()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type in [
                TokenType.MOD,
                TokenType.INT_DIV,
                TokenType.FLOAT_DIV,
                TokenType.MUL,
            ]:
                op = self.currTok
                self.eat()  # eat * / // %
                rhs = self.expo()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleEndToks:
                return lhs
            else:
                self.error('binary operator or ")" or "]" or ":=" or "," or newline')

    """
    expo -> expo "^" elem 
        | elem
    """

    def expo(self):
        possibleEndToks = [
            TokenType.MOD,
            TokenType.INT_DIV,
            TokenType.FLOAT_DIV,
            TokenType.MUL,
            TokenType.MINUS,
            TokenType.PLUS,
            TokenType.GT,
            TokenType.GE,
            TokenType.LT,
            TokenType.LE,
            TokenType.EQ,
            TokenType.NE,
            TokenType.AND,
            TokenType.OR,
            TokenType.EOL,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.ASSIGN,
            TokenType.COMMA,
        ]

        lhs = self.elem()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type == TokenType.EXP:
                op = self.currTok
                self.eat()  # eat ^
                rhs = self.elem()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleEndToks:
                return lhs
            else:
                self.error('binary operator or ")" or "]" or ":=" or "," or newline')

    """
    elem -> "-" elem
        | "~" elem
        | "(" expr ")"
        | ID
        | ID ":" args EOL
        | ID "[" expr "]"
        | REAL_LIT
        | BOOL_LIT
        | STRING_LIT

        and reserved keywords
    """

    def elem(self):
        if self.currTok.type == TokenType.ID:
            tok = self.currTok
            ident = self.currTok.lexVal
            identAST = IdentifierASTNode(
                token=tok, type=None, ident=ident, index=None, listFlag=False
            )
            self.eat()  # eat id
            if self.currTok.type == TokenType.COLON:
                self.eat()  # eat :
                args = self.args()
                if self.currTok.type == TokenType.EOL:
                    self.eat()  # eat \n
                    if args:
                        return CallExprASTNode(
                            token=tok, callee=identAST, args=args
                        )  # id: args
                    else:
                        return CallExprASTNode(
                            token=tok, callee=identAST, args=[]
                        )  # id:
                else:
                    self.error("newline")
            elif self.currTok.type == TokenType.LSQUARE:
                self.eat()  # eat [
                index = self.expr()
                if self.currTok.type == TokenType.RSQUARE:
                    self.eat()  # eat ]
                    if index:
                        identAST.set_index(index)
                        identAST.set_listFlag(True)
                        return identAST  # id[expr]
                    else:
                        self.error("valid expression as index")
                else:
                    self.error('"]"')
            else:
                return identAST  # id
        elif self.currTok.type == TokenType.DISPLAY:
            tok = self.currTok
            keyWordAST = ReservedKeyWordASTNode(
                token=tok, value=print, ident=self.currTok.lexVal
            )
            self.eat()  # eat 'display'
            if self.currTok.type == TokenType.COLON:
                self.eat()  # eat :
                args = self.args()
                if self.currTok.type == TokenType.EOL:
                    self.eat()  # eat \n
                    if args:
                        return CallExprASTNode(
                            token=tok, callee=keyWordAST, args=args
                        )  # display: args
                    else:
                        return CallExprASTNode(
                            token=tok, callee=identAST, args=[]
                        )  # display:
                else:
                    self.error("newline")
            else:
                self.error('":"')
        elif self.currTok.type == TokenType.LENGTH:
            tok = self.currTok
            keyWordAST = ReservedKeyWordASTNode(
                token=tok, value=len, ident=self.currTok.lexVal
            )
            self.eat()  # eat 'len'
            if self.currTok.type == TokenType.COLON:
                self.eat()  # eat :
                args = self.args()
                if self.currTok.type == TokenType.EOL:
                    self.eat()  # eat \n
                    if args:
                        return CallExprASTNode(
                            token=tok, callee=keyWordAST, args=args
                        )  # len: args
                else:
                    self.error("newline")
            else:
                self.error('":"')
        elif self.currTok.type == TokenType.MINUS or self.currTok.type == TokenType.NOT:
            op = self.currTok
            self.eat()  # eat - ~
            rhs = self.elem()
            if rhs:
                return UnaryExprASTNode(op=op, rhs=rhs)
        elif self.currTok.type == TokenType.LPAREN:
            self.eat()  # eat (
            expr = self.expr()
            if self.currTok.type == TokenType.RPAREN:
                self.eat()  # eat )
                if expr:
                    return expr
            else:
                self.error('")"')
        elif self.currTok.type == TokenType.REAL_LIT:
            realNum = RealASTNode(token=self.currTok, value=self.currTok.lexVal)
            self.eat()  # eat real number
            return realNum
        elif self.currTok.type == TokenType.BOOL_LIT:
            boolVal = BoolASTNode(token=self.currTok, value=self.currTok.lexVal)
            self.eat()  # eat boolean value
            return boolVal
        elif self.currTok.type == TokenType.STRING_LIT:
            strVal = StringASTNode(token=self.currTok, value=self.currTok.lexVal)
            self.eat()  # eat string value
            return strVal
        elif self.currTok.type == TokenType.NULL:
            nullVal = ReservedKeyWordASTNode(
                token=self.currTok, value=None, ident=self.currTok.lexVal
            )
            self.eat()  # eat 'null'
            return nullVal
        elif self.currTok.type == TokenType.PI:
            piVal = ReservedKeyWordASTNode(
                token=self.currTok, value=math.pi, ident=self.currTok.lexVal
            )
            self.eat()  # eat 'pi'
            return piVal
        elif self.currTok.type == TokenType.EULER:
            eulVal = ReservedKeyWordASTNode(
                token=self.currTok, value=math.e, ident=self.currTok.lexVal
            )
            self.eat()  # eat 'eul'
            return eulVal
        else:
            self.error(
                'identifier or unary expression or "(" or real, bool or string literal'
            )

    """
    args -> arg_list
        | epsilon
    """

    def args(self):
        possibleStartToks = [
            TokenType.ID,
            TokenType.MINUS,
            TokenType.NOT,
            TokenType.LPAREN,
            TokenType.REAL_LIT,
            TokenType.BOOL_LIT,
            TokenType.STRING_LIT,
            TokenType.NULL,
            TokenType.PI,
            TokenType.EULER,
        ]

        if self.currTok.type in possibleStartToks:  # id: args or id := [args]
            argList = self.arg_list()
            if argList:
                return argList
        elif (
            self.currTok.type == TokenType.EOL or self.currTok.type == TokenType.RSQUARE
        ):  # id: or id []
            return self.epsilon()
        else:
            self.error("list of expressins as arguments or newline")

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
                self.eat()  # eat ,
                expr = self.expr()
                if expr:
                    argList.append(expr)
            elif (
                self.currTok.type == TokenType.EOL
                or self.currTok.type == TokenType.RSQUARE
            ):
                return argList
            else:
                self.error('"," or newline')

    """
    epsilon -> 
    """

    def epsilon(self):
        return None
