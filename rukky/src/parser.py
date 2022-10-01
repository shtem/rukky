from common.errors import ParserError
from src.lexer import Lexer
from data.ast import *
import random
import math
import sys


class Parser:
    def __init__(self, lexer: Lexer):
        self.lexer = lexer
        self.prevTok: Token = None
        self.currTok: Token = None

    def error(self, missing: str):
        print(
            ParserError(
                message=f'Unexpected Token "{repr(self.currTok)}". Expected {missing}',
            )
        )
        sys.exit(0)

    def reset(self):
        self.lexer.reset()
        self.prevTok: Token = None
        self.currTok: Token = None

    def eat(self):
        # eat current token by assigning current token to next token in the input
        self.prevTok = self.currTok
        self.currTok = self.lexer.get_next_token()

    def peek(self):
        return self.lexer.peek_next_token()

    def parse(self):
        self.eat()  # set current token to first token in the input
        return self.program()

    """
    program -> decl_list EOF
    """

    def program(self):
        declList = self.decl_list()

        if self.currTok.type == TokenType.EOF:
            self.eat()  # eat EOF
            if declList:
                return ProgramASTNode(declarList=declList)
            else:
                return self.epsilon()

    """
    decl_list -> decl_list decl
            | decl
    """

    def decl_list(self):
        possibleStartToks = [
            TokenType.ID,
            TokenType.MINUS,
            TokenType.NOT,
            TokenType.LPAREN,
            TokenType.LSQUARE,
            TokenType.LBRACE,
            TokenType.REAL_LIT,
            TokenType.BOOL_LIT,
            TokenType.STRING_LIT,
            TokenType.NULL,
            TokenType.IF,
            TokenType.FOR,
            TokenType.WHILE,
            TokenType.IN,
            TokenType.RETURN,
            TokenType.BREAK,
            TokenType.CONTINUE,
            TokenType.VOID,
            TokenType.REAL,
            TokenType.BOOL,
            TokenType.STRING,
            TokenType.OBJECT,
            TokenType.DISPLAY,
            TokenType.LENGTH,
            TokenType.TYPE,
            TokenType.STRINGIFY,
            TokenType.REALIFY,
            TokenType.MAX,
            TokenType.MIN,
            TokenType.RANDOM,
            TokenType.FLOOR,
            TokenType.CEIL,
            TokenType.SQRT,
            TokenType.LOG,
            TokenType.SIN,
            TokenType.COS,
            TokenType.TAN,
            TokenType.PI,
            TokenType.EULER,
            TokenType.RES_COLON,
            TokenType.EOL,
        ]

        declList = []

        decl = self.decl()
        if decl:
            declList.append(decl)

        while True:
            if self.currTok.type in possibleStartToks:
                decl = self.decl()
                if decl:
                    declList.append(decl)
            elif self.currTok.type == TokenType.EOF:
                return declList
            else:
                self.error(
                    'expression or "if", "while" or "for" statement or newline or "real", "bool", "str" or "obj" or "::"'
                )

    """
    decl -> stmt 
        | func_decl
    """

    def decl(self):
        if self.currTok.type == TokenType.RES_COLON:
            return self.func_decl()
        elif self.currTok.type == TokenType.EOF:
            return self.epsilon()
        else:
            return self.stmt()

    """
    var_type -> "real"
            | "bool"
            | "str"
            | "obj"
    """

    def var_type(self):
        if self.currTok.type == TokenType.REAL:
            self.eat()  # eat 'real'
            return "real"
        elif self.currTok.type == TokenType.BOOL:
            self.eat()  # eat 'bool'
            return "bool"
        elif self.currTok.type == TokenType.STRING:
            self.eat()  # eat 'str'
            return "str"
        elif self.currTok.type == TokenType.OBJECT:
            self.eat()  # eat 'obj'
            return "obj"
        else:
            self.error('"real" or "bool" or "str" or "obj"')

    """
    func_type -> "void"
                | decl_type
    """

    def func_type(self):
        if self.currTok.type == TokenType.VOID:
            self.eat()  # eat 'void'
            return "void", False  # can't have void arr so always false
        elif self.currTok.type in [
            TokenType.REAL,
            TokenType.BOOL,
            TokenType.STRING,
            TokenType.OBJECT,
        ]:
            return self.decl_type()
        else:
            self.error('"void" or "real" or "bool" or "str" or "obj"')

    """
    func_decl -> "::" func_type ID ":=" "(" params ")" block 
    """

    def func_decl(self):
        if self.currTok.type == TokenType.RES_COLON:
            tok = self.currTok
            self.eat()  # eat ::
            fType, isReturnList = self.func_type()

            if fType:
                if self.currTok.type == TokenType.ID:
                    ident = self.currTok.lexVal
                    self.eat()  # eat id
                    funcName = IdentifierASTNode(
                        token=tok,
                        type=fType,
                        ident=ident,
                        index=None,
                        arrFlag=isReturnList,
                        mapFlag=False,
                    )
                    if self.currTok.type == TokenType.ASSIGN:
                        self.eat()  # eat :=
                        if self.currTok.type == TokenType.LPAREN:
                            self.eat()  # eat (
                            params = self.params()
                            if self.currTok.type == TokenType.RPAREN:
                                self.eat()  # eat )
                                body = self.block()
                                if body:
                                    return FunctionASTNode(
                                        token=tok,
                                        funcName=funcName,
                                        params=params,
                                        funcBody=body,
                                    )
                                else:
                                    return FunctionASTNode(
                                        token=tok,
                                        funcName=funcName,
                                        params=params,
                                        funcBody=StmtBlockASTNode(
                                            token=self.currTok, stmtList=[]
                                        ),
                                    )
                            else:
                                self.error('")"')
                        else:
                            self.error('"("')
                    else:
                        self.error('":="')
                else:
                    self.error("identifier")
            else:
                return self.epsilon()
        else:
            self.error('"::"')

    """
    params -> param_list
            | epsilon
    """

    def params(self):
        if self.currTok.type in [
            TokenType.REAL,
            TokenType.BOOL,
            TokenType.STRING,
            TokenType.OBJECT,
        ]:  # func := (params)
            paramList = self.param_list()
            if paramList:
                return paramList
        elif self.currTok.type == TokenType.RPAREN:  # func := ()
            return []
        else:
            self.error('"real" or "bool" or "str" or "obj" or ")"')

        return []

    """
    param_list -> param_list "," param
                | param
    """

    def param_list(self):
        paramList = []

        param = self.param()
        if param:
            paramList.append(param)

            while True:
                if self.currTok.type == TokenType.COMMA:
                    self.eat()  # eat ,
                    param = self.param()
                    if param:
                        paramList.append(param)
                elif self.currTok.type == TokenType.RPAREN:
                    return paramList
                else:
                    self.error('"," or ")"')

        return []

    """
    param -> decl_type ID
    """

    def param(self):
        tok = self.currTok
        pType, isArr = self.decl_type()

        if pType:
            if self.currTok.type == TokenType.ID:
                ident = self.currTok.lexVal
                self.eat()  # eat id
                return IdentifierASTNode(
                    token=tok,
                    type=pType,
                    ident=ident,
                    index=None,
                    arrFlag=isArr,
                    mapFlag=False,
                )
            else:
                self.error("identifier")
        else:
            return self.epsilon()

    """
    block -> "{" EOL stmt_list "}" EOL
            | "{" EOL "}" EOL
            | "{}" EOL
    """

    def block(self):
        possibleStartToks = [
            TokenType.ID,
            TokenType.MINUS,
            TokenType.NOT,
            TokenType.LPAREN,
            TokenType.LSQUARE,
            TokenType.LBRACE,
            TokenType.REAL_LIT,
            TokenType.BOOL_LIT,
            TokenType.STRING_LIT,
            TokenType.NULL,
            TokenType.IF,
            TokenType.FOR,
            TokenType.WHILE,
            TokenType.IN,
            TokenType.RETURN,
            TokenType.BREAK,
            TokenType.CONTINUE,
            TokenType.REAL,
            TokenType.BOOL,
            TokenType.STRING,
            TokenType.OBJECT,
            TokenType.DISPLAY,
            TokenType.LENGTH,
            TokenType.TYPE,
            TokenType.STRINGIFY,
            TokenType.REALIFY,
            TokenType.MAX,
            TokenType.MIN,
            TokenType.RANDOM,
            TokenType.FLOOR,
            TokenType.CEIL,
            TokenType.SQRT,
            TokenType.LOG,
            TokenType.SIN,
            TokenType.COS,
            TokenType.TAN,
            TokenType.PI,
            TokenType.EULER,
            TokenType.EOL,
        ]

        if self.currTok.type == TokenType.LBRACE:
            tok = self.currTok
            self.eat()  # eat {
            if self.currTok.type == TokenType.EOL or self.currTok.type == TokenType.EOF:
                if self.peek().type in possibleStartToks:
                    self.eat()  # eat \n
                    stmtList = self.stmt_list()
                    if self.currTok.type == TokenType.RBRACE:
                        self.eat()  # eat }
                        if (
                            self.currTok.type == TokenType.EOL
                            or self.currTok.type == TokenType.EOF
                        ):
                            self.eat()  # eat \n
                            if stmtList:
                                return StmtBlockASTNode(token=tok, stmtList=stmtList)
                            else:
                                return StmtBlockASTNode(token=tok, stmtList=[])
                        else:
                            self.error("newline")
                    else:
                        self.error('"}"')
                elif self.peek().type == TokenType.RBRACE:
                    self.eat()  # eat \n
                    self.eat()  # eat }
                    if (
                        self.currTok.type == TokenType.EOL
                        or self.currTok.type == TokenType.EOF
                    ):
                        self.eat()  # eat \n
                        return self.epsilon()  # {\n} empty block
                    else:
                        self.error("newline")
                else:
                    self.error(
                        'expression or "if", "while", "for", "return", "break" or "continue" statement or newline or "}"'
                    )
            elif self.currTok.type == TokenType.RBRACE:
                self.eat()  # eat }
                if (
                    self.currTok.type == TokenType.EOL
                    or self.currTok.type == TokenType.EOF
                ):
                    self.eat()  # eat \n
                    return self.epsilon()  # {} empty block
                else:
                    self.error("newline")
            else:
                self.error('newline or "}"')
        else:
            self.error('"{"')

    """
    stmt_list -> stmt_list stmt
                | stmt
    """

    def stmt_list(self):
        possibleStartToks = [
            TokenType.ID,
            TokenType.MINUS,
            TokenType.NOT,
            TokenType.LPAREN,
            TokenType.LSQUARE,
            TokenType.LBRACE,
            TokenType.REAL_LIT,
            TokenType.BOOL_LIT,
            TokenType.STRING_LIT,
            TokenType.NULL,
            TokenType.IF,
            TokenType.FOR,
            TokenType.WHILE,
            TokenType.IN,
            TokenType.RETURN,
            TokenType.BREAK,
            TokenType.CONTINUE,
            TokenType.REAL,
            TokenType.BOOL,
            TokenType.STRING,
            TokenType.OBJECT,
            TokenType.DISPLAY,
            TokenType.LENGTH,
            TokenType.TYPE,
            TokenType.STRINGIFY,
            TokenType.REALIFY,
            TokenType.MAX,
            TokenType.MIN,
            TokenType.RANDOM,
            TokenType.FLOOR,
            TokenType.CEIL,
            TokenType.SQRT,
            TokenType.LOG,
            TokenType.SIN,
            TokenType.COS,
            TokenType.TAN,
            TokenType.PI,
            TokenType.EULER,
            TokenType.EOL,
        ]

        stmtList = []

        stmt = self.stmt()
        if stmt:
            stmtList.append(stmt)

        while True:
            if self.currTok.type in possibleStartToks:
                stmt = self.stmt()
                if stmt:
                    stmtList.append(stmt)
            elif self.currTok.type == TokenType.RBRACE:
                return stmtList
            else:
                self.error(
                    'expression or "if", "while", "for", "return", "break" or "continue" statement or newline or "real", "bool", "str" or "obj" or "}"'
                )

    """
    stmt -> decl_stmt
        | expr_stmt
        | for_stmt
        | while_stmt
        | in_stmt
        | if_stmt
        | return_stmt
        | break_stmt
        | cont_stmt
    """

    def stmt(self):
        possibleStartToks = [
            TokenType.ID,
            TokenType.MINUS,
            TokenType.NOT,
            TokenType.LPAREN,
            TokenType.LSQUARE,
            TokenType.LBRACE,
            TokenType.REAL_LIT,
            TokenType.BOOL_LIT,
            TokenType.STRING_LIT,
            TokenType.NULL,
            TokenType.DISPLAY,
            TokenType.LENGTH,
            TokenType.TYPE,
            TokenType.STRINGIFY,
            TokenType.REALIFY,
            TokenType.MAX,
            TokenType.MIN,
            TokenType.RANDOM,
            TokenType.FLOOR,
            TokenType.CEIL,
            TokenType.SQRT,
            TokenType.LOG,
            TokenType.SIN,
            TokenType.COS,
            TokenType.TAN,
            TokenType.PI,
            TokenType.EULER,
            TokenType.EOL,
        ]

        if self.currTok.type == TokenType.FOR:
            return self.for_stmt()
        elif self.currTok.type == TokenType.WHILE:
            return self.while_stmt()
        elif self.currTok.type == TokenType.IN:
            return self.in_stmt()
        elif self.currTok.type == TokenType.IF:
            return self.if_stmt()
        elif self.currTok.type == TokenType.RETURN:
            return self.return_stmt()
        elif self.currTok.type == TokenType.BREAK:
            return self.break_stmt()
        elif self.currTok.type == TokenType.CONTINUE:
            return self.cont_stmt()
        elif self.currTok.type in [
            TokenType.REAL,
            TokenType.BOOL,
            TokenType.STRING,
            TokenType.OBJECT,
        ]:
            return self.decl_stmt()
        elif self.currTok.type in possibleStartToks:
            return self.expr_stmt()
        elif self.currTok.type == TokenType.RBRACE:
            return self.epsilon()
        else:
            return self.error(
                'expression or "if", "while", "for", "return", "break" or "continue" statement or newline or "real", "bool", "str" or "obj"'
            )

    """
    decl_type -> var_type
                | var_type "[]"
    """

    def decl_type(self):
        vType = self.var_type()

        if vType == TokenType.OBJECT.value and self.currTok.type == TokenType.LSQUARE:
            self.error("identifier")  # don't want obj[] y := [..] just obj y := [..]

        if vType:
            if self.currTok.type == TokenType.LSQUARE:
                self.eat()  # eat [
                if self.currTok.type == TokenType.RSQUARE:
                    self.eat()  # eat ]
                    return vType, True  # var_type[]
                else:
                    self.error('"]"')
            else:
                return vType, False  # var_type
        else:
            return None, False

    """
    decl_stmt -> decl_type ID EOL
            | decl_type ID ":=" expr EOL
    """

    def decl_stmt(self):
        tok = self.currTok
        vType, isArr = self.decl_type()

        if vType:
            if self.currTok.type == TokenType.ID:
                if (
                    self.peek().type == TokenType.EOL
                    or self.peek().type == TokenType.EOF
                ):
                    ident = self.currTok.lexVal
                    self.eat()  # eat id
                    identAST = IdentifierASTNode(
                        token=tok,
                        type=vType,
                        ident=ident,
                        index=None,
                        arrFlag=isArr,
                        mapFlag=False,
                    )
                    self.eat()  # eat \n
                    return identAST
                elif self.peek().type == TokenType.ASSIGN:
                    ident = self.currTok.lexVal
                    self.eat()  # eat id
                    identAST = IdentifierASTNode(
                        token=tok,
                        type=vType,
                        ident=ident,
                        index=None,
                        arrFlag=isArr,
                        mapFlag=False,
                    )
                    self.eat()  # eat :=
                    val = self.expr()
                    if (
                        self.currTok.type == TokenType.EOL
                        or self.currTok.type == TokenType.EOF
                    ):
                        self.eat()  # eat \n
                        if val:
                            if isinstance(val, MapASTNode):
                                identAST.set_map_flag(True)
                            return AssignASTNode(token=tok, var=identAST, value=val)
                    else:
                        self.error("newline")
                else:
                    self.error('newline or ":="')
            else:
                self.error("identifier")
        else:
            return self.epsilon()

    """
    expr_stmt -> expr EOL
            | EOL
    """

    def expr_stmt(self):
        if self.currTok.type == TokenType.EOL or self.currTok.type == TokenType.EOF:
            self.eat()  # eat \n
            return self.epsilon()
        else:
            expr = self.expr()
            if self.currTok.type == TokenType.EOL or self.currTok.type == TokenType.EOF:
                self.eat()  # eat \n
                if expr:
                    return expr
            else:
                self.error("newline")

    """
    for_stmt -> "for" "::" ID ":=" expr ";" expr ";" expr block
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
                        arrFlag=False,
                        mapFlag=False,
                    )
                    self.eat()  # eat id
                    if self.currTok.type == TokenType.ASSIGN:
                        self.eat()  # eat :=
                        start = self.expr()
                        if self.currTok.type == TokenType.SEM_COLON:
                            self.eat()  # eat ;
                            end = self.expr()
                            if self.currTok.type == TokenType.SEM_COLON:
                                self.eat()  # eat ;
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
    in_stmt -> "in" "::" "(" ID "," ID ")" ";" ID block
    """

    def in_stmt(self):
        if self.currTok.type == TokenType.IN:
            tok = self.currTok
            self.eat()  # eat 'in'
            if self.currTok.type == TokenType.RES_COLON:
                self.eat()  # eat ::
                if self.currTok.type == TokenType.LPAREN:
                    self.eat()  # eat (
                    if self.currTok.type == TokenType.ID:
                        identOneAST = IdentifierASTNode(
                            token=self.currTok,
                            type="obj",
                            ident=self.currTok.lexVal,
                            index=None,
                            arrFlag=False,
                            mapFlag=False,
                        )
                        self.eat()  # eat id
                        if self.currTok.type == TokenType.COMMA:
                            self.eat()  # eat ,
                            if self.currTok.type == TokenType.ID:
                                identTwoAST = IdentifierASTNode(
                                    token=self.currTok,
                                    type="obj",
                                    ident=self.currTok.lexVal,
                                    index=None,
                                    arrFlag=False,
                                    mapFlag=False,
                                )
                                self.eat()  # eat id
                                if self.currTok.type == TokenType.RPAREN:
                                    self.eat()  # eat )
                                    if self.currTok.type == TokenType.SEM_COLON:
                                        self.eat()  # eat ;
                                        if self.currTok.type == TokenType.ID:
                                            arrMapAST = IdentifierASTNode(
                                                token=self.currTok,
                                                type=None,
                                                ident=self.currTok.lexVal,
                                                index=None,
                                                arrFlag=False,
                                                mapFlag=False,
                                            )
                                            self.eat()  # eat id
                                            body = self.block()
                                            if body:
                                                return InStmtASTNode(token=tok, keyIdent=identOneAST, valueIdent=identTwoAST, arrMapIdent=arrMapAST, inBody=body)
                                        else:
                                            self.error("identifier")
                                    else:
                                        self.error('";"')
                                else:
                                    self.error('")"')
                            else:
                                self.error("identifier")
                        else:
                            self.error('","')
                    else:
                        self.error("identifier")
                else:
                    self.error('"("')
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
            TokenType.LSQUARE,
            TokenType.LBRACE,
            TokenType.REAL_LIT,
            TokenType.BOOL_LIT,
            TokenType.STRING_LIT,
            TokenType.NULL,
            TokenType.RBRACE,
            TokenType.IF,
            TokenType.ELSE,
            TokenType.FOR,
            TokenType.WHILE,
            TokenType.IN,
            TokenType.RETURN,
            TokenType.BREAK,
            TokenType.CONTINUE,
            TokenType.VOID,
            TokenType.REAL,
            TokenType.BOOL,
            TokenType.STRING,
            TokenType.OBJECT,
            TokenType.DISPLAY,
            TokenType.LENGTH,
            TokenType.TYPE,
            TokenType.STRINGIFY,
            TokenType.REALIFY,
            TokenType.MAX,
            TokenType.MIN,
            TokenType.RANDOM,
            TokenType.FLOOR,
            TokenType.CEIL,
            TokenType.SQRT,
            TokenType.LOG,
            TokenType.SIN,
            TokenType.COS,
            TokenType.TAN,
            TokenType.PI,
            TokenType.EULER,
            TokenType.EOL,
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

        return []

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
    return_stmt -> "return" "::" EOL
                | "return" "::" expr EOL
    """

    def return_stmt(self):
        if self.currTok.type == TokenType.RETURN:
            tok = self.currTok
            self.eat()  # eat 'return'
            if self.currTok.type == TokenType.RES_COLON:
                self.eat()  # eat ::
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
                self.error('"::"')
        else:
            return self.epsilon()

    """
    break_stmt -> "break" "::" EOL
    """

    def break_stmt(self):
        if self.currTok.type == TokenType.BREAK:
            tok = self.currTok
            self.eat()  # eat 'break'
            if self.currTok.type == TokenType.RES_COLON:
                self.eat()  # eat ::
                if self.currTok.type == TokenType.EOL:
                    self.eat()  # eat \n
                    return BreakStmtASTNode(token=tok)
                else:
                    self.error("newline")
            else:
                self.error('"::"')
        else:
            return self.epsilon()

    """
    cont_stmt -> "continue" "::" EOL
    """

    def cont_stmt(self):
        if self.currTok.type == TokenType.CONTINUE:
            tok = self.currTok
            self.eat()  # eat 'continue'
            if self.currTok.type == TokenType.RES_COLON:
                self.eat()  # eat ::
                if self.currTok.type == TokenType.EOL:
                    self.eat()  # eat \n
                    return ContinueStmtASTNode(token=tok)
                else:
                    self.error("newline")
            else:
                self.error('"::"')
        else:
            return self.epsilon()

    """
    expr -> ID ":=" expr
        | ID "@" expr ":=" expr
        | ID "<<" expr
        | disjunc
    """

    def expr(self):
        if self.currTok.type == TokenType.ID:

            if self.prevTok:
                if (
                    self.prevTok.type == TokenType.LIST_ASSIGN
                    and self.peek().type == TokenType.ASSIGN
                ):
                    return self.disjunc()  # catch case where id@id := expr

            tok = self.currTok
            if self.peek().type == TokenType.ASSIGN:
                ident = tok.lexVal
                self.eat()  # eat id
                identAST = IdentifierASTNode(
                    token=tok,
                    type=None,
                    ident=ident,
                    index=None,
                    arrFlag=False,
                    mapFlag=False,
                )
                self.eat()  # eat :=
                val = self.expr()
                if val:
                    if isinstance(val, ArrayASTNode):
                        identAST.set_arr_flag(True)
                    if isinstance(val, MapASTNode):
                        identAST.set_map_flag(True)
                    return AssignASTNode(token=tok, var=identAST, value=val)
            elif self.peek().type == TokenType.APPEND:
                ident = tok.lexVal
                self.eat()  # eat id
                identAST = IdentifierASTNode(
                    token=tok,
                    type=None,
                    ident=ident,
                    index=None,
                    arrFlag=True,
                    mapFlag=False,
                )
                op = self.currTok
                self.eat()  # eat <<
                val = self.expr()
                if val:
                    return BinaryExprASTNode(op=op, lhs=identAST, rhs=val)
            elif self.peek().type == TokenType.LIST_ASSIGN:
                ident = tok.lexVal
                self.eat()  # eat id
                identAST = IdentifierASTNode(
                    token=tok,
                    type=None,
                    ident=ident,
                    index=None,
                    arrFlag=True,
                    mapFlag=False,
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
            TokenType.EOF,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.RBRACE,
            TokenType.ASSIGN,
            TokenType.MAP_ASSIGN,
            TokenType.COMMA,
            TokenType.COLON,
            TokenType.SEM_COLON,
            TokenType.RES_COLON,
            TokenType.LBRACE,
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
                self.error(
                    'binary operator or ")" or "]" or "{" or ":=" or "->" or "," or ":" or "::" or newline'
                )

    """
    conjunc -> conjunc "&&" equiv
        | equiv
    """

    def conjunc(self):
        possibleEndToks = [
            TokenType.OR,
            TokenType.EOL,
            TokenType.EOF,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.RBRACE,
            TokenType.ASSIGN,
            TokenType.MAP_ASSIGN,
            TokenType.COMMA,
            TokenType.COLON,
            TokenType.SEM_COLON,
            TokenType.RES_COLON,
            TokenType.LBRACE,
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
                self.error(
                    'binary operator or ")" or "]" or "{" or ":=" or "->" or "," or ":" or "::" or newline'
                )

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
            TokenType.EOF,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.RBRACE,
            TokenType.ASSIGN,
            TokenType.MAP_ASSIGN,
            TokenType.COMMA,
            TokenType.COLON,
            TokenType.SEM_COLON,
            TokenType.RES_COLON,
            TokenType.LBRACE,
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
                self.error(
                    'binary operator or ")" or "]" or "{" or ":=" or "->" or "," or ":" or "::" or newline'
                )

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
            TokenType.EOF,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.RBRACE,
            TokenType.ASSIGN,
            TokenType.MAP_ASSIGN,
            TokenType.COMMA,
            TokenType.COLON,
            TokenType.SEM_COLON,
            TokenType.RES_COLON,
            TokenType.LBRACE,
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
                self.error(
                    'binary operator or ")" or "]" or "{" or ":=" or "->" or "," or ":" or "::" or newline'
                )

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
            TokenType.EOF,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.RBRACE,
            TokenType.ASSIGN,
            TokenType.MAP_ASSIGN,
            TokenType.COMMA,
            TokenType.COLON,
            TokenType.SEM_COLON,
            TokenType.RES_COLON,
            TokenType.LBRACE,
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
                self.error(
                    'binary operator or ")" or "]" or "{" or ":=" or "->" or "," or ":" or "::" or newline'
                )

    """
    factor -> factor "*" pow
        | factor "/" pow
        | factor "//" pow
        | factor "%" pow
        | pow
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
            TokenType.EOF,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.RBRACE,
            TokenType.ASSIGN,
            TokenType.MAP_ASSIGN,
            TokenType.COMMA,
            TokenType.COLON,
            TokenType.SEM_COLON,
            TokenType.RES_COLON,
            TokenType.LBRACE,
        ]

        lhs = self.pow()

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
                rhs = self.pow()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleEndToks:
                return lhs
            else:
                self.error(
                    'binary operator or ")" or "]" or "{" or ":=" or "->" or "," or ":" or "::" or newline'
                )

    """
    pow -> pow "^" elem 
        | elem
    """

    def pow(self):
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
            TokenType.EOF,
            TokenType.RPAREN,
            TokenType.RSQUARE,
            TokenType.RBRACE,
            TokenType.ASSIGN,
            TokenType.MAP_ASSIGN,
            TokenType.COMMA,
            TokenType.COLON,
            TokenType.SEM_COLON,
            TokenType.RES_COLON,
            TokenType.LBRACE,
        ]

        lhs = self.elem()

        if not lhs:
            return self.epsilon()

        while True:
            if self.currTok.type == TokenType.POW:
                op = self.currTok
                self.eat()  # eat ^
                rhs = self.elem()
                if rhs:
                    lhs = BinaryExprASTNode(op=op, lhs=lhs, rhs=rhs)
            elif self.currTok.type in possibleEndToks:
                return lhs
            else:
                self.error(
                    'binary operator or ")" or "]" or "{" or ":=" or "->" or "," or ":" or "::" or newline'
                )

    """
    elem -> "-" elem
        | "~" elem
        | "(" expr ")"
        | "[" args "]"
        | "{" map "}"
        | ID
        | ID ":" args "::"
        | ID "[" expr "]"
        | REAL_LIT
        | BOOL_LIT
        | STRING_LIT
    """

    def elem(self):
        if self.currTok.type == TokenType.ID:
            tok = self.currTok
            ident = self.currTok.lexVal
            identAST = IdentifierASTNode(
                token=tok,
                type=None,
                ident=ident,
                index=None,
                arrFlag=False,
                mapFlag=False,
            )
            self.eat()  # eat id
            if self.currTok.type == TokenType.COLON:
                self.eat()  # eat :
                args = self.args()
                if self.currTok.type == TokenType.RES_COLON:
                    self.eat()  # eat ::
                    if args:
                        return CallExprASTNode(
                            token=tok, callee=identAST, args=args
                        )  # id: args::
                    else:
                        return CallExprASTNode(
                            token=tok, callee=identAST, args=[]
                        )  # id: :: make a function call with no arguments
                else:
                    self.error('"::"')
            elif self.currTok.type == TokenType.LSQUARE:
                self.eat()  # eat [
                index = self.expr()
                if self.currTok.type == TokenType.RSQUARE:
                    self.eat()  # eat ]
                    if index:
                        identAST.set_index(index)
                        return identAST  # id[expr]
                    else:
                        self.error("valid expression as index")
                else:
                    self.error('"]"')
            else:
                return identAST  # id
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
        elif self.currTok.type == TokenType.LSQUARE:
            arrTok = self.currTok
            self.eat()  # eat [
            elems = self.args()
            if self.currTok.type == TokenType.RSQUARE:
                self.eat()  # eat ]
                if elems:
                    return ArrayASTNode(token=arrTok, elems=elems)
                else:
                    return ArrayASTNode(token=arrTok, elems=[])
            else:
                self.error('"]"')
        elif self.currTok.type == TokenType.LBRACE:
            mapTok = self.currTok
            self.eat()  # eat {
            elems = self.map()
            if self.currTok.type == TokenType.RBRACE:
                self.eat()  # eat }
                if elems:
                    return MapASTNode(token=mapTok, elems=elems)
                else:
                    return MapASTNode(token=mapTok, elems=[])
            else:
                self.error('"]"')
        elif self.currTok.type == TokenType.REAL_LIT:
            realNum = RealASTNode(token=self.currTok, value=self.currTok.lexVal)
            self.eat()  # eat real number
            return realNum
        elif self.currTok.type == TokenType.BOOL_LIT:
            val = 1 if self.currTok.lexVal == "true" else 0
            boolVal = BoolASTNode(token=self.currTok, value=val)
            self.eat()  # eat boolean value
            return boolVal
        elif self.currTok.type == TokenType.STRING_LIT:
            strVal = StringASTNode(token=self.currTok, value=self.currTok.lexVal)
            self.eat()  # eat string value
            return strVal
        else:
            return self._reserved_keywords()

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
            TokenType.LSQUARE,
            TokenType.LBRACE,
            TokenType.REAL_LIT,
            TokenType.BOOL_LIT,
            TokenType.STRING_LIT,
            TokenType.NULL,
            TokenType.LENGTH,
            TokenType.TYPE,
            TokenType.STRINGIFY,
            TokenType.REALIFY,
            TokenType.MAX,
            TokenType.MIN,
            TokenType.RANDOM,
            TokenType.FLOOR,
            TokenType.CEIL,
            TokenType.SQRT,
            TokenType.LOG,
            TokenType.SIN,
            TokenType.COS,
            TokenType.TAN,
            TokenType.PI,
            TokenType.EULER,
        ]

        if self.currTok.type in possibleStartToks:  # id: args: or id := [args]
            argList = self.args_list()
            if argList:
                return argList
        elif (
            self.currTok.type == TokenType.RES_COLON
            or self.currTok.type == TokenType.RSQUARE
        ):  # id: :: or id := []
            return []
        else:
            self.error('list of expressions as arguments or "::" or "]"')

        return []

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
                    self.currTok.type == TokenType.RES_COLON
                    or self.currTok.type == TokenType.RSQUARE
                ):
                    return argList
                else:
                    self.error('"," or "]" or "::"')

        return []

    """
    map -> pair_list
        | epsilon
    """

    def map(self):
        possibleStartToks = [
            TokenType.ID,
            TokenType.MINUS,
            TokenType.NOT,
            TokenType.LPAREN,
            TokenType.LSQUARE,
            TokenType.LBRACE,
            TokenType.REAL_LIT,
            TokenType.BOOL_LIT,
            TokenType.STRING_LIT,
            TokenType.NULL,
            TokenType.LENGTH,
            TokenType.TYPE,
            TokenType.STRINGIFY,
            TokenType.REALIFY,
            TokenType.MAX,
            TokenType.MIN,
            TokenType.RANDOM,
            TokenType.FLOOR,
            TokenType.CEIL,
            TokenType.SQRT,
            TokenType.LOG,
            TokenType.SIN,
            TokenType.COS,
            TokenType.TAN,
            TokenType.PI,
            TokenType.EULER,
        ]

        if self.currTok.type in possibleStartToks:  # id := {map}
            pairList = self.pair_list()
            if pairList:
                return pairList
        elif self.currTok.type == TokenType.RBRACE:  # id := {}
            return []
        else:
            self.error('list of expression pairs or "}"')

        return []

    """
    pair_list -> pair_list "," pair 
        | pair 
    """

    def pair_list(self):
        pairList = []

        pair = self.pair()
        if pair:
            pairList.append(pair)

            while True:
                if self.currTok.type == TokenType.COMMA:
                    self.eat()  # eat ,
                    pair = self.pair()
                    if pair:
                        pairList.append(pair)
                elif self.currTok.type == TokenType.RBRACE:
                    return pairList
                else:
                    self.error('"," or "}"')

        return []

    """
    pair -> expr "->" expr
    """

    def pair(self):
        keyExpr = self.expr()

        if keyExpr:
            if self.currTok.type == TokenType.MAP_ASSIGN:
                self.eat()  # eat ->
                valExpr = self.expr()
                if valExpr:
                    return (keyExpr, valExpr)
        else:
            return self.epsilon()

    """
    epsilon -> 
    """

    def epsilon(self):
        return None

    """
    Handles reserved keywords and functions
    """

    def _reserved_keywords(self):
        mulTypes = (str, float, int, bool, list, dict)
        keyWordToFuncDict = {
            TokenType.DISPLAY: {
                "value": print,
                "argNum": 1,
                "returnType": type(None),
                "argType": mulTypes,
            },
            TokenType.LENGTH: {
                "value": len,
                "argNum": 1,
                "returnType": (int, float),
                "argType": (str, list),
            },
            TokenType.TYPE: {
                "value": type,
                "argNum": 2,
                "returnType": bool,
                "argType": mulTypes,
            },
            TokenType.STRINGIFY: {
                "value": str,
                "argNum": 1,
                "returnType": str,
                "argType": mulTypes,
            },
            TokenType.REALIFY: {
                "value": float,
                "argNum": 1,
                "returnType": float,
                "argType": mulTypes[:4],
            },
            TokenType.MAX: {
                "value": max,
                "argNum": 1,
                "returnType": float,
                "argType": list,
            },
            TokenType.MIN: {
                "value": min,
                "argNum": 1,
                "returnType": float,
                "argType": list,
            },
            TokenType.RANDOM: {
                "value": random.random,
                "argNum": 0,
                "returnType": float,
                "argType": None,
            },
            TokenType.FLOOR: {
                "value": math.floor,
                "argNum": 1,
                "returnType": (int, float),
                "argType": (int, float),
            },
            TokenType.CEIL: {
                "value": math.ceil,
                "argNum": 1,
                "returnType": (int, float),
                "argType": (int, float),
            },
            TokenType.SQRT: {
                "value": math.sqrt,
                "argNum": 1,
                "returnType": float,
                "argType": (int, float),
            },
            TokenType.LOG: {
                "value": math.log,
                "argNum": 1,
                "returnType": float,
                "argType": (int, float),
            },
            TokenType.SIN: {
                "value": math.sin,
                "argNum": 1,
                "returnType": float,
                "argType": (int, float),
            },
            TokenType.COS: {
                "value": math.cos,
                "argNum": 1,
                "returnType": float,
                "argType": (int, float),
            },
            TokenType.TAN: {
                "value": math.tan,
                "argNum": 1,
                "returnType": float,
                "argType": (int, float),
            },
        }

        if self.currTok.type in list(keyWordToFuncDict.keys()):
            tok = self.currTok
            funcMap = keyWordToFuncDict.get(self.currTok.type)
            keyWordAST = ReservedKeyWordASTNode(
                token=tok,
                ident=self.currTok.lexVal,
                value=funcMap["value"],
                isFunc=True,
                returnType=funcMap["returnType"],
                argNum=funcMap["argNum"],
                argType=funcMap["argType"],
            )
            self.eat()  # eat reserved function keyword
            if self.currTok.type == TokenType.COLON:
                self.eat()  # eat :
                args = self.args()
                if self.currTok.type == TokenType.RES_COLON:
                    self.eat()  # eat ::
                    if args:
                        return CallExprASTNode(
                            token=tok, callee=keyWordAST, args=args
                        )  # keyword: args::
                    else:
                        return CallExprASTNode(
                            token=tok, callee=keyWordAST, args=[]
                        )  # keyword: ::
                else:
                    self.error('"::"')
            else:
                self.error('":"')
        elif self.currTok.type == TokenType.NULL:
            nullVal = ReservedKeyWordASTNode(
                token=self.currTok,
                ident=self.currTok.lexVal,
                value=None,
                isFunc=False,
            )
            self.eat()  # eat 'null'
            return nullVal
        elif self.currTok.type == TokenType.PI:
            piVal = ReservedKeyWordASTNode(
                token=self.currTok,
                ident=self.currTok.lexVal,
                value=math.pi,
                isFunc=False,
            )
            self.eat()  # eat 'pi'
            return piVal
        elif self.currTok.type == TokenType.EULER:
            eulVal = ReservedKeyWordASTNode(
                token=self.currTok,
                ident=self.currTok.lexVal,
                value=math.e,
                isFunc=False,
            )
            self.eat()  # eat 'eul'
            return eulVal
        else:
            self.error(
                'identifier or unary expression or "(" or "[" real, bool or string literal'
            )
