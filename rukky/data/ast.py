from abc import ABC, abstractmethod
from data.token import Token


class ASTNode(ABC):
    def __init__(self):
        self.level = 0

    def update_level(self):
        self.level = self.level + 1

    @abstractmethod
    def __str__(self):
        pass

    def __repr__(self):
        self.update_level()
        return self.__str__()

    @abstractmethod
    def code_gen(self):
        pass


class StmtASTNode(ASTNode):
    pass


class ExprASTNode(StmtASTNode):
    pass


class RealASTNode(ExprASTNode):
    def __init__(self, token: Token, value: str):
        super().__init__()
        self.token = token
        self.value = float(value)

    def __str__(self):
        return f"-> RealASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.value}"

    def code_gen(self):
        pass


class BoolASTNode(ExprASTNode):
    def __init__(self, token: Token, value: str):
        super().__init__()
        self.token = token
        self.value = bool(value)

    def __str__(self):
        return f"-> BoolASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.value}"

    def code_gen(self):
        pass


class StringASTNode(ExprASTNode):
    def __init__(self, token: Token, value: str):
        super().__init__()
        self.token = token
        self.value = str(value)

    def __str__(self):
        return f"-> StringASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.value}"

    def code_gen(self):
        pass


class IdentifierASTNode(ExprASTNode):
    def __init__(
        self, token: Token, type: str, ident: str, index: ExprASTNode, listFlag: bool
    ):
        super().__init__()
        self.token = token
        self.type = type
        self.ident = ident
        self.index = index
        self.listFlag = listFlag

    def __str__(self):
        out = f"-> IdentifierASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.ident} {self.type if self.type else ''}{list() if self.listFlag else ''} "
        if self.index:
            self.index.level = self.level + 3
            out += f"\n{' ' * (self.level)}@-¬"
            out += f"\n{' ' * (self.level + 3)}-{repr(self.index)}"

        return out

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

    def code_gen(self):
        pass


class ReservedKeyWordASTNode(IdentifierASTNode):
    def __init__(
        self,
        token: Token,
        ident: str,
        value,
    ):
        super().__init__(
            token=token, type=None, ident=ident, index=None, listFlag=False
        )
        self.value = value

    def __str__(self):
        return f"-> ReservedKeyWordASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.ident} {self.value}"

    def code_gen(self):
        pass


class UnaryExprASTNode(ExprASTNode):
    def __init__(self, op: Token, rhs: ExprASTNode):
        super().__init__()
        self.op = op
        self.rhs = rhs

    def __str__(self):
        self.rhs.level = self.level
        return f"-> UnaryExprASTNode (lineNo={self.op.lineNo}, columnNo={self.op.columnNo}) {self.op.lexVal}\n{' ' * (self.level)}-{repr(self.rhs)}"

    def code_gen(self):
        pass


class BinaryExprASTNode(ExprASTNode):
    def __init__(self, op: Token, lhs: ExprASTNode, rhs: ExprASTNode):
        super().__init__()
        self.op = op
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self):
        self.rhs.level = self.level
        self.lhs.level = self.level
        return f"-> BinaryExprASTNode (lineNo={self.op.lineNo}, columnNo={self.op.columnNo}) {self.op.lexVal}\n{' ' * (self.level)}-{repr(self.lhs)}\n{' ' * (self.level)}-{repr(self.rhs)}"

    def code_gen(self):
        pass


class CallExprASTNode(ExprASTNode):
    def __init__(
        self, token: Token, callee: IdentifierASTNode, args: list[ExprASTNode]
    ):
        super().__init__()
        self.token = token
        self.callee = callee
        self.args = args

    def __str__(self):
        self.callee.level = self.level
        out = f"-> CallExprASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n{' ' * (self.level)}-{repr(self.callee)} "
        if self.args:
            out += f"\n{' ' * (self.level)}()-¬"
            for arg in self.args:
                arg.level = self.level + 3
                out += f"\n{' ' * (self.level + 3)}-{repr(arg)}"

        return out

    def code_gen(self):
        pass


class ListASTNode(ExprASTNode):
    def __init__(self, token: Token, elems: list[ExprASTNode]):
        super().__init__()
        self.token = token
        self.elems = elems

    def __str__(self):
        out = f"-> ListASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) "
        if self.elems:
            for elem in self.elems:
                elem.level = self.level
                out += f"\n{' ' * (self.level)}-{repr(elem)}"

        return out

    def code_gen(self):
        pass


class AssignASTNode(ExprASTNode):
    def __init__(self, token: Token, var: IdentifierASTNode, value: ExprASTNode):
        super().__init__()
        self.token = token
        self.var = var
        self.value = value

    def __str__(self):
        self.var.level = self.level
        self.value.level = self.level + 3
        out = f"-> AssignASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n{' ' * (self.level)}-{repr(self.var)} "
        out += f"\n{' ' * (self.level)}:=-¬"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.value)}"

        return out

    def get_var(self):
        return self.var

    def code_gen(self):
        pass


class StmtBlockASTNode(StmtASTNode):
    def __init__(self, token: Token, stmtList: list[StmtASTNode]):
        super().__init__()
        self.token = token
        self.stmtList = stmtList

    def __str__(self):
        out = f"-> StmtBlockASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) "
        for stmt in self.stmtList:
            stmt.level = self.level
            out += f"\n{' ' * (self.level)}-{repr(stmt)}"

        return out

    def code_gen(self):
        pass


class ElifStmtASTNode(StmtASTNode):
    def __init__(self, token: Token, cond: ExprASTNode, elifBody: StmtBlockASTNode):
        super().__init__()
        self.token = token
        self.cond = cond
        self.elifBody = elifBody

    def __str__(self):
        self.cond.level = self.level + 3
        self.elifBody.level = self.level
        out = f"-> ElifStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) "
        out += f"\n{' ' * (self.level)}?-¬"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.cond)}"
        out += f"\n{' ' * (self.level)}-{repr(self.elifBody)}"

        return out

    def code_gen(self):
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
        super().__init__()
        self.token = token
        self.cond = cond
        self.ifBody = ifBody
        self.elifStmts = elifStmts
        self.elseBody = elseBody

    def __str__(self):
        self.cond.level = self.level + 3
        self.ifBody.level = self.level
        out = f"-> IfStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) "
        out += f"\n{' ' * (self.level)}?-¬"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.cond)}"
        out += f"\n{' ' * (self.level)}-{repr(self.ifBody)}"
        if self.elifStmts:
            for el in self.elifStmts:
                el.level = self.level
                out += f"\n{' ' * (self.level)}-{repr(el)}"
        if self.elseBody:
            self.elseBody.level = self.level
            out += f"\n{' ' * (self.level)}-{repr(self.elseBody)}"

        return out

    def code_gen(self):
        pass


class WhileStmtASTNode(StmtASTNode):
    def __init__(self, token: Token, cond: ExprASTNode, whileBody: StmtBlockASTNode):
        super().__init__()
        self.token = token
        self.cond = cond
        self.whileBody = whileBody

    def __str__(self):
        self.cond.level = self.level + 3
        out = f"-> WhileStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) "
        out += f"\n{' ' * (self.level)}?-¬"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.cond)}"
        if self.whileBody:
            self.whileBody.level = self.level
            out += f"\n{' ' * (self.level)}-{repr(self.whileBody)}"

        return out

    def code_gen(self):
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
        super().__init__()
        self.token = token
        self.counter = counter
        self.start = start
        self.end = end
        self.increment = increment
        self.forBody = forBody

    def __str__(self):
        self.counter.level = self.level
        self.start.level = self.level + 3
        self.end.level = self.level + 3
        self.increment.level = self.level + 3
        out = f"-> ForStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n{' ' * (self.level)}-{repr(self.counter)} "
        out += f"\n{' ' * (self.level)}#-¬"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.start)}"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.end)}"
        out += f"\n{' ' * (self.level + 3)}-{repr(self.increment)}"
        if self.forBody:
            self.forBody.level = self.level
            out += f"\n{' ' * (self.level)}-{repr(self.forBody)}"

        return out

    def code_gen(self):
        pass


class ReturnStmtASTNode(StmtASTNode):
    def __init__(self, token: Token, returnBody: ExprASTNode):
        super().__init__()
        self.token = token
        self.returnBody = returnBody

    def __str__(self):
        out = f"-> ReturnStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})"
        if self.returnBody:
            self.returnBody.level = self.level
            out += f"\n{' ' * (self.level)}-{repr(self.returnBody)}"

        return out

    def code_gen(self):
        pass


class BreakStmtASTNode(StmtASTNode):
    def __init__(self, token: Token):
        super().__init__()
        self.token = token

    def __str__(self):
        return f"-> BreakStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})"

    def code_gen(self):
        pass


class FunctionASTNode(ASTNode):
    def __init__(
        self,
        token: Token,
        funcName: IdentifierASTNode,
        params: list[IdentifierASTNode],
        funcBody: StmtBlockASTNode,
    ):
        super().__init__()
        self.token = token
        self.funcName = funcName
        self.params = params
        self.funcBody = funcBody

    def __str__(self):
        self.funcName.level = self.level
        out = f"-> FunctionASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})\n{' ' * (self.level)}-{repr(self.funcName)} "
        if self.params:
            out += f"\n{' ' * (self.level)}()-¬"
            for par in self.params:
                par.level = self.level + 3
                out += f"\n{' ' * (self.level + 3)}-{repr(par)}"
        if self.funcBody:
            self.funcBody.level = self.level
            out += f"\n{' ' * (self.level)}-{repr(self.funcBody)}"

        return out

    def code_gen(self):
        pass


class ProgramASTNode(ASTNode):
    def __init__(self, declarList: list[ASTNode]):
        super().__init__()
        self.declarList = declarList

    def __str__(self):
        out = f"|-> ProgramASTNode "
        if self.declarList:
            for decl in self.declarList:
                decl.level = self.level
                out += f"\n{' ' * (self.level)}-{repr(decl)}"
        out +="\n<-|"

        return out

    def code_gen(self):
        pass
