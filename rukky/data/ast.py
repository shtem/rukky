from abc import ABC, abstractmethod
from data.token import Token


class ASTNode(ABC):
    @abstractmethod
    def __str__(self):
        pass

    @abstractmethod
    def codegen(self):
        pass


class StmtASTNode(ASTNode):
    pass


class ExprASTNode(StmtASTNode):
    pass


class RealASTNode(ExprASTNode):
    def __init__(self, token: Token, value: str):
        self.token = token
        self.value = float(value)

    def __str__(self):
        return f"-> RealASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.value}"

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class BoolASTNode(ExprASTNode):
    def __init__(self, token: Token, value: str):
        self.token = token
        self.value = bool(value)

    def __str__(self):
        return f"-> BoolASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.value}"

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class StringASTNode(ExprASTNode):
    def __init__(self, token: Token, value: str):
        self.token = token
        self.value = str(value)

    def __str__(self):
        return f"-> StringASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.value}"

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class IdentifierASTNode(ExprASTNode):
    def __init__(
        self, token: Token, type: str, ident: str, index: ExprASTNode, listFlag: bool
    ):
        self.token = token
        self.type = type
        self.ident = ident
        self.index = index
        self.listFlag = listFlag

    def __str__(self):
        return f"-> IdentifierASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.ident} {self.type}{list() if self.listFlag else None} {self.index}"

    def __repr__(self):
        return self.__str__()

    def get_ident(self):
        return self.ident

    def get_type(self):
        return self.type

    def set_index(self, i):
        self.index = i

    def set_listFlag(self, flag):
        self.listFlag = flag

    def is_list(self):
        return self.listFlag

    def codegen(self):
        pass


class ReservedKeyWordASTNode(IdentifierASTNode):
    def __init__(
        self,
        token: Token,
        ident: str,
        value,
    ):
        self.token = token
        self.ident = ident
        self.value = value

    def __str__(self):
        return f"-> ReservedKeyWordASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.ident} {self.value}"

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class UnaryExprASTNode(ExprASTNode):
    def __init__(self, op: Token, rhs: ExprASTNode):
        self.op = op
        self.rhs = rhs

    def __str__(self):
        return f"-> UnaryExprASTNode (lineNo={self.op.lineNo}, columnNo={self.op.columnNo}) {self.op.lexVal}\n\t-{repr(self.rhs)}"

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class BinaryExprASTNode(ExprASTNode):
    def __init__(self, op: Token, lhs: ExprASTNode, rhs: ExprASTNode):
        self.op = op
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self):
        return f"-> BinaryExprASTNode (lineNo={self.op.lineNo}, columnNo={self.op.columnNo}) {self.op.lexVal}\n\t-{repr(self.lhs)}\n\t-{repr(self.rhs)}"

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class CallExprASTNode(ExprASTNode):
    def __init__(
        self, token: Token, callee: IdentifierASTNode, args: list[ExprASTNode]
    ):
        self.token = token
        self.callee = callee
        self.args = args

    def __str__(self):
        out = f"-> CallExprASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n\t-{repr(self.callee)} "
        for arg in self.args:
            out += f"\n\t-{repr(arg)}"

        return out

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class ListASTNode(ExprASTNode):
    def __init__(self, token: Token, elems: list[ExprASTNode]):
        self.token = token
        self.elems = elems

    def __str__(self):
        out = f"-> ListASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) "
        for elem in self.elems:
            out += f"\n\t-{repr(elem)}"

        return out

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class AssignASTNode(ExprASTNode):
    def __init__(self, token: Token, var: IdentifierASTNode, value: ExprASTNode):
        self.token = token
        self.var = var
        self.value = value

    def __str__(self):
        return f"-> AssignASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n\t-{repr(self.var)}\n\t-{repr(self.value)}"

    def __repr__(self):
        return self.__str__()

    def get_var(self):
        return self.var

    def codegen(self):
        pass


class StmtBlockASTNode(StmtASTNode):
    def __init__(self, token: Token, stmtList: list[StmtASTNode]):
        self.token = token
        self.stmtList = stmtList

    def __str__(self):
        out = f"-> StmtBlockASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) "
        for stmt in self.stmtList:
            out += f"\n\t-{repr(stmt)}"

        return out

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class ElifStmtASTNode(StmtASTNode):
    def __init__(self, token: Token, cond: ExprASTNode, elifBody: StmtBlockASTNode):
        self.token = token
        self.cond = cond
        self.elifBody = elifBody

    def __str__(self):
        return f"-> ElifStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n\t-{repr(self.cond)}\n\t-{repr(self.elifBody)} "

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class IfStmtASTNode(StmtASTNode):
    def __init__(
        self,
        token: Token,
        cond: ExprASTNode,
        ifBody: StmtBlockASTNode,
        elifStmts: list[ElifStmtASTNode],
        elseBody: StmtBlockASTNode,
    ):
        self.token = token
        self.cond = cond
        self.ifBody = ifBody
        self.elifStmts = elifStmts
        self.elseBody = elseBody

    def __str__(self):
        out = f"-> IfStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n\t-{repr(self.cond)}\n\t-{repr(self.ifBody)} "
        if self.elifStmts:
            for el in self.elifStmts:
                out += f"\n\t-{repr(el)}"
        if self.elseBody:
            out += f"\n\t-{repr(self.elseBody)}"

        return out

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class WhileStmtASTNode(StmtASTNode):
    def __init__(self, token: Token, cond: ExprASTNode, whileBody: StmtBlockASTNode):
        self.token = token
        self.cond = cond
        self.whileBody = whileBody

    def __str__(self):
        out = f"-> WhileStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n\t-{repr(self.cond)} "
        if self.whileBody:
            out += f"\n\t-{repr(self.whileBody)}"

        return out

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class ForStmtASTNode(StmtASTNode):
    def __init__(
        self,
        token: Token,
        counter: IdentifierASTNode,
        start: ExprASTNode,
        end: ExprASTNode,
        increment: ExprASTNode,
        forBody: StmtBlockASTNode,
    ):
        self.token = token
        self.counter = counter
        self.start = start
        self.end = end
        self.increment = increment
        self.forBody = forBody

    def __str__(self):
        out = f"-> ForStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n\t-{repr(self.counter)}\n\t-{repr(self.start)}\n\t-{repr(self.end)}\n\t-{repr(self.increment)} "
        if self.forBody:
            out += f"\n\t-{repr(self.forBody)}"

        return out

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class ReturnStmtASTNode(StmtASTNode):
    def __init__(self, token: Token, returnBody: ExprASTNode):
        self.token = token
        self.returnBody = returnBody

    def __str__(self):
        out = f"-> ReturnStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})"
        if self.returnBody:
            out += f"\n\t-{repr(self.returnBody)}"

        return out

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class BreakStmtASTNode(StmtASTNode):
    def __init__(self, token: Token):
        self.token = token

    def __str__(self):
        return f"-> BreakStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})"

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class FunctionASTNode(ASTNode):
    def __init__(
        self,
        token: Token,
        funcName: IdentifierASTNode,
        params: list[IdentifierASTNode],
        funcBody: StmtBlockASTNode,
    ):
        self.token = token
        self.funcName = funcName
        self.params = params
        self.funcBody = funcBody

    def __str__(self):
        out = f"-> FunctionASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n\t-{repr(self.funcName)}\n\t-{repr(self.params)} "
        if self.funcBody:
            out += f"\n\t-{repr(self.funcBody)}"

        return out

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass


class ProgramASTNode(ASTNode):
    def __init__(self, declarList: list[ASTNode]):
        self.declarList = declarList

    def __str__(self):
        out = f"-> ProgramASTNode "
        for decl in self.declarList:
            out += f"\n\t-{repr(decl)}"

        return out

    def __repr__(self):
        return self.__str__()

    def codegen(self):
        pass
