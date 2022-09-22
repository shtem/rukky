from common.lex_enums import TokenType
from data.context import TheContext, FuncEntry
from data.token import Token
from abc import ABC, abstractmethod


class ASTNode(ABC):
    def __init__(self):
        self.level = 0

    def update_level(self):
        self.level = self.level + 1

    def __repr__(self):
        self.update_level()
        return self.__str__()

    @abstractmethod
    def __str__(self):
        pass

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

    def code_gen(self, context: TheContext):
        return self.value


class BoolASTNode(ExprASTNode):
    def __init__(self, token: Token, value: int):
        super().__init__()
        self.token = token
        self.value = bool(value)

    def __str__(self):
        return f"-> BoolASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.value}"

    def code_gen(self, context: TheContext):
        return self.value


class StringASTNode(ExprASTNode):
    def __init__(self, token: Token, value: str):
        super().__init__()
        self.token = token
        self.value = str(value)

    def __str__(self):
        return f"-> StringASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.value}"

    def code_gen(self, context: TheContext):
        return self.value


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

    def set_list_flag(self, flag):
        self.listFlag = flag

    def is_list(self):
        return self.listFlag

    def determine_type_value(self):
        if self.type == "real":
            return type(0.0), 0.0
        elif self.type == "bool":
            return type(False), False
        elif self.type == "str":
            return type(""), ""
        elif self.type == "void":  # only applies to functions
            return type(None), None
        else:
            raise TypeError

    def code_gen(self, context: TheContext):
        symbol = self.ident
        if (
            self.type
        ):  # variable declaration: type != None e.g. var_type ID .. or var_type[] ID ..
            type, defaultValue = self.determine_type_value()

            if self.listFlag:
                defaultValue = []

            context.set_ident(
                symbol=symbol,
                valType=type,
                value=defaultValue,
                index=None,
                isAppend=False,
                isList=self.listFlag,
            )

            return defaultValue
        else:  # variable retrieval: type = None e.g. ID or ID[expr]
            valType = context.get_ident_type(symbol=symbol, getList=False)
            if not valType:
                raise ValueError  # variable does not exist

            if self.index:
                indexVal = self.index.code_gen(context=context)
                if context.is_real(indexVal):
                    varValue = context.get_ident(
                        symbol=symbol, index=int(indexVal), getList=self.listFlag
                    )
                else:
                    raise TypeError
            else:
                isList = isinstance([], valType) if valType else False
                varValue = context.get_ident(symbol=symbol, index=None, getList=isList)

            return varValue


class ReservedKeyWordASTNode(IdentifierASTNode):
    def __init__(
        self,
        token: Token,
        ident: str,
        value,
        isFunc=False,
        returnType=None,
        argNum=0,
        argType=None,
    ):
        super().__init__(
            token=token, type=None, ident=ident, index=None, listFlag=False
        )
        self.value = value
        self.isFunc = isFunc
        self.returnType = returnType
        self.argNum = argNum
        self.argType = argType

    def __str__(self):
        return f"-> ReservedKeyWordASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo}) {self.ident} {self.value}"

    def generate_builtin(self, context: TheContext):
        symbol = self.get_ident()
        isReturnList = False
        returnType = self.returnType

        argSymbols = []

        funcContext = TheContext(parent=context)
        funcContext.inFunc = True

        for par in range(self.argNum):
            argSymbols.append(str(par))  # store parameter variable names
            # add parameters as variables to function context, don't care about list content type for display and stringify
            funcContext.set_ident(
                symbol=str(par),
                valType=self.argType,
                value=None,
                index=None,
                isAppend=False,
                isList=False,
            )

        context.set_func(
            symbol=symbol,
            returnType=returnType,
            argSymbols=argSymbols,
            funcBody=context._type_builtin
            if symbol == TokenType.TYPE.value
            else self.value,
            context=funcContext,
            isReturnList=isReturnList,
        )

    def code_gen(self, context: TheContext):
        if self.isFunc:
            self.generate_builtin(context=context)
            return None
        else:
            return self.value


class UnaryExprASTNode(ExprASTNode):
    def __init__(self, op: Token, rhs: ExprASTNode):
        super().__init__()
        self.op = op
        self.rhs = rhs

    def __str__(self):
        self.rhs.level = self.level
        return f"-> UnaryExprASTNode (lineNo={self.op.lineNo}, columnNo={self.op.columnNo}) {self.op.lexVal}\n{' ' * (self.level)}-{repr(self.rhs)}"

    def code_gen(self, context: TheContext):
        rVal = self.rhs.code_gen(context=context)

        if rVal == None:
            raise ValueError

        match self.op.type:
            case TokenType.MINUS:
                if context.is_real(value=rVal):
                    return -rVal
                else:
                    raise TypeError  # incompatible types
            case TokenType.NOT:
                if context.is_bool(value=rVal):
                    return not rVal
                else:
                    raise TypeError
            case _:
                raise ValueError  # invalid operator


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

    def code_gen(self, context: TheContext):
        lVal = self.lhs.code_gen(context=context)
        rVal = self.rhs.code_gen(context=context)

        if (
            lVal == None
            and not isinstance(
                self.lhs, (ReservedKeyWordASTNode, IdentifierASTNode, CallExprASTNode)
            )
        ) or (
            rVal == None
            and not isinstance(
                self.rhs, (ReservedKeyWordASTNode, IdentifierASTNode, CallExprASTNode)
            )
        ):
            raise ValueError  # allow for null checks

        match self.op.type:
            case TokenType.PLUS:
                if (
                    context.type_checker(left=lVal, right=rVal)
                    and lVal != None
                    and rVal != None
                ):
                    result = lVal + rVal  # allows for addition and concatenation
                    if context.is_real(value=result):
                        return float(result)
                    else:
                        return result
                else:
                    raise TypeError  # incompatible types
            case TokenType.MINUS:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return float(lVal - rVal)
                else:
                    raise TypeError
            case TokenType.MUL:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return float(lVal * rVal)
                else:
                    raise TypeError
            case TokenType.FLOAT_DIV:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return float(lVal / rVal)
                else:
                    raise TypeError
            case TokenType.INT_DIV:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return float(lVal // rVal)
                else:
                    raise TypeError
            case TokenType.MOD:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return float(lVal % rVal)
                else:
                    raise TypeError
            case TokenType.EXP:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return float(lVal**rVal)
                else:
                    raise TypeError
            case TokenType.AND:
                if context.is_bool(value=lVal) and context.is_bool(value=rVal):
                    return bool(lVal and rVal)
                else:
                    raise TypeError
            case TokenType.OR:
                if context.is_bool(value=lVal) and context.is_bool(value=rVal):
                    return bool(lVal or rVal)
                else:
                    raise TypeError
            case TokenType.GT:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return bool(lVal > rVal)
                else:
                    raise TypeError
            case TokenType.GE:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return bool(lVal >= rVal)
                else:
                    raise TypeError
            case TokenType.LT:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return bool(lVal < rVal)
                else:
                    raise TypeError
            case TokenType.LE:
                if context.is_real(value=lVal) and context.is_real(value=rVal):
                    return bool(lVal <= rVal)
                else:
                    raise TypeError
            case TokenType.EQ:
                if context.type_checker(left=lVal, right=rVal):
                    return bool(lVal == rVal)
                else:
                    raise TypeError
            case TokenType.NE:
                if context.type_checker(left=lVal, right=rVal):
                    return bool(lVal != rVal)
                else:
                    raise TypeError
            case TokenType.APPEND:
                if isinstance(self.lhs, IdentifierASTNode):
                    if self.lhs.is_list():
                        ident = self.lhs.get_ident()
                        context.set_ident(
                            symbol=ident,
                            valType=None,
                            value=rVal,
                            index=None,
                            isAppend=True,
                            isList=True,
                        )
                        return context.get_ident(symbol=ident, index=None, getList=True)
                    else:
                        raise TypeError
                else:
                    raise TypeError
            case _:
                raise ValueError  # invalid operator


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

    def execute_builtin(self, fEntry: FuncEntry):
        # retrieve args values from context of reserved call and pass to inbuilt function
        argVals = (
            fEntry.context.get_ident(symbol=arg, index=None, getList=False)
            for arg in fEntry.argSymbols
        )
        rVal = fEntry.funcBody(*argVals)
        fEntry.context.funcReturnVal = rVal

    def code_gen(self, context: TheContext):
        if self.callee == None:
            raise ValueError

        if isinstance(self.callee, ReservedKeyWordASTNode):
            self.callee.code_gen(context=context)  # generate builtin function

        symbol = self.callee.get_ident()
        fEntry = context.get_func(symbol=symbol)

        if fEntry == None:
            raise ValueError  # function doesn't exist/ has not yet been defined

        if len(self.args) != len(fEntry.argSymbols):
            raise ValueError  # incorrect number of arguments

        context.inFunc = True  # inside function

        argValues = [arg.code_gen(context=context) for arg in self.args]
        argValSymb = zip(fEntry.argSymbols, argValues)

        for (
            par,
            val,
        ) in (
            argValSymb
        ):  # for each argument value try assigning to parameter to check types in function context
            if fEntry.context.type_checker_assign(left=par, right=val, hasIndex=False):
                fEntry.context.set_ident(
                    symbol=par,
                    valType=None,
                    value=val,
                    index=None,
                    isAppend=False,
                    isList=fEntry.context.get_ident_type(symbol=par, getList=False)
                    == type([]),
                )
            else:
                raise TypeError

        if isinstance(self.callee, ReservedKeyWordASTNode):
            self.execute_builtin(fEntry=fEntry)
        else:
            funcReturn = fEntry.funcBody.code_gen(
                context=fEntry.context
            )  # execute function body using function context
            fEntry.context.funcReturnVal = funcReturn

        funcContext: TheContext = fEntry.context

        if fEntry.type_checker_return():
            rVal = funcContext.funcReturnVal
            fEntry.context.reset_flags()
            if context.is_real(value=rVal) and not context.is_bool(value=rVal):
                return float(rVal)
            else:
                return rVal
        else:
            raise TypeError  # function type doesn't match return type


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

    def code_gen(self, context: TheContext):
        listVal = [elem.code_gen(context=context) for elem in self.elems]

        return listVal


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

    def code_gen(self, context: TheContext):
        if self.var.get_type():
            self.var.code_gen(context=context)

        assignVal = self.value.code_gen(context=context)
        symbol = self.var.get_ident()

        if assignVal == None and not isinstance(
            self.value, (ReservedKeyWordASTNode, IdentifierASTNode, CallExprASTNode)
        ):
            raise ValueError

        if self.var.index:
            if context.type_checker_assign(left=symbol, right=assignVal, hasIndex=True):
                indexVal = self.var.index.code_gen(context=context)
                context.set_ident(
                    symbol=symbol,
                    valType=None,
                    value=assignVal,
                    index=int(indexVal),
                    isAppend=False,
                    isList=self.var.listFlag,
                )
            else:
                raise TypeError
        else:
            if context.type_checker_assign(
                left=symbol, right=assignVal, hasIndex=False
            ):
                context.set_ident(
                    symbol=symbol,
                    valType=None,
                    value=assignVal,
                    index=None,
                    isAppend=False,
                    isList=self.var.listFlag,
                )
            else:
                raise TypeError

        return assignVal


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

    def code_gen(self, context: TheContext):
        if not self.stmtList:
            raise ValueError  # block empty

        stmtVal = None
        for decl in self.stmtList:
            stmtVal = decl.code_gen(context=context)

            if context.should_return():
                context.inFunc = False
                return stmtVal

        return stmtVal


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

    def code_gen(self, context: TheContext):
        if self.cond == None or self.elifBody == None:
            raise ValueError

        condVal = self.cond.code_gen(context=context)

        if not context.is_bool(value=condVal):
            raise TypeError

        return condVal, self.elifBody


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

    def code_gen(self, context: TheContext):
        if self.cond == None or self.ifBody == None:
            raise ValueError  # empty block

        condVal = self.cond.code_gen(context=context)

        if not context.is_bool(value=condVal):
            raise TypeError  # invalid boolean expression

        if condVal:  # if condition true evaluate if body
            bVal = self.ifBody.code_gen(context=context)
            return bVal

        if self.elifStmts:
            for el in self.elifStmts:
                elCond, elBody = el.code_gen(context=context)
                if elCond:
                    bVal = elBody.code_gen(context=context)
                    return bVal

        if self.elseBody:
            bVal = self.elseBody.code_gen(context=context)
            return bVal

        return None


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

    def code_gen(self, context: TheContext):
        if self.cond == None or self.whileBody == None:
            raise ValueError

        context.inLoop = True

        while True:
            condVal = self.cond.code_gen(context=context)
            if not context.is_bool(value=condVal):
                raise TypeError

            if not condVal:
                break

            bVal = self.whileBody.code_gen(context=context)
            context.inLoop = True

            if context.should_return() and not context.breakFlag:
                context.inLoop = False
                return bVal

            if context.should_break():
                context.inLoop = False
                context.breakFlag = False
                break

        context.inLoop = False

        return bVal


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

    def code_gen(self, context: TheContext):
        if (
            self.counter == None
            or self.start == None
            or self.end == None
            or self.increment == None
            or self.forBody == None
        ):
            raise ValueError

        context.inLoop = True

        self.counter.code_gen(context=context)
        symbol = self.counter.get_ident()

        sVal = self.start.code_gen(context=context)
        endVal = self.end.code_gen(context=context)
        incVal = self.increment.code_gen(context=context)

        if (
            not context.is_real(value=sVal)
            or not context.is_real(value=endVal)
            or not context.is_real(value=incVal)
        ):
            raise TypeError

        i = sVal

        if incVal == 0:
            raise ValueError  # invalid increment value

        if incVal >= 0:
            for_cond = lambda: i < endVal
        else:
            for_cond = lambda: i > endVal

        context.set_ident(
            symbol=symbol,
            valType=None,
            value=sVal,
            index=None,
            isAppend=False,
            isList=False,
        )

        while for_cond():
            i += incVal
            bVal = self.forBody.code_gen(context=context)
            context.inLoop = True

            if context.should_return() and not context.breakFlag:
                context.inLoop = False
                return bVal

            if context.should_break():
                context.inLoop = False
                context.breakFlag = False
                break

            context.set_ident(
                symbol=symbol,
                valType=None,
                value=i,
                index=None,
                isAppend=False,
                isList=False,
            )

        context.inLoop = False

        return bVal


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

    def code_gen(self, context: TheContext):
        if not context.inFunc:
            raise ValueError  # trying to return outside a function

        context.returnFlag = True

        if self.returnBody:
            rVal = self.returnBody.code_gen(context=context)
            context.funcReturnVal = rVal

        return rVal


class BreakStmtASTNode(StmtASTNode):
    def __init__(self, token: Token):
        super().__init__()
        self.token = token

    def __str__(self):
        return f"-> BreakStmtASTNode (lineNo={self.token.lineNo}, columnNo={self.token.columnNo})"

    def code_gen(self, context: TheContext):
        if not context.inLoop:
            raise ValueError  # trying to break outside a loop

        context.breakFlag = True

        return None


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

    def code_gen(self, context: TheContext):
        if self.funcName == None and self.funcBody == None:
            raise ValueError

        symbol = self.funcName.get_ident()
        isReturnList = self.funcName.is_list()
        returnType, _ = self.funcName.determine_type_value()

        argSymbols = []

        funcContext = TheContext(parent=context)
        funcContext.inFunc = True

        for par in self.params:
            argSymbols.append(par.get_ident())  # store parameter variable names
            par.code_gen(
                context=funcContext
            )  # add parameters as variables to function context

        context.set_func(
            symbol=symbol,
            returnType=returnType,
            argSymbols=argSymbols,
            funcBody=self.funcBody,
            context=funcContext,
            isReturnList=isReturnList,
        )

        return None


class ProgramASTNode(ASTNode):
    def __init__(self, declarList: list[ASTNode]):
        super().__init__()
        self.declarList = declarList
        self.programContext = TheContext(parent=None)

    def __str__(self):
        out = f"|-> ProgramASTNode "
        if self.declarList:
            for decl in self.declarList:
                decl.level = self.level
                out += f"\n{' ' * (self.level)}-{repr(decl)}"
        out += "\n<-|"

        return out

    def code_gen(self):
        progVal = None
        for decl in self.declarList:
            progVal = decl.code_gen(context=self.programContext)
            self.programContext.inFunc = False

        return progVal
